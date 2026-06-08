import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { createServer as createNetServer } from 'node:net';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright-core';
import { slugTitle } from '../src/org.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, '..');
const HEADFUL = process.env.PLAYWRIGHT_HEADFUL === '1';

const explicitChromePath = process.env.PLAYWRIGHT_CHROME || process.env.CHROME_PATH;

function log(message) {
  process.stdout.write(`${message}\n`);
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function freePort() {
  return new Promise((resolve, reject) => {
    const server = createNetServer();
    server.on('error', reject);
    server.listen(0, '127.0.0.1', () => {
      const address = server.address();
      server.close(() => resolve(address.port));
    });
  });
}

async function waitForServer(baseUrl, server) {
  const deadline = Date.now() + 12_000;
  let lastError = null;
  while (Date.now() < deadline) {
    if (server.exitCode !== null) {
      throw new Error(`Server exited before readiness with code ${server.exitCode}`);
    }
    try {
      const response = await fetch(`${baseUrl}/api/state`, { cache: 'no-store' });
      if (response.ok) return;
      lastError = new Error(`HTTP ${response.status}`);
    } catch (error) {
      lastError = error;
    }
    await sleep(120);
  }
  throw new Error(`Server did not become ready: ${lastError?.message || 'timeout'}`);
}

function launchOptions() {
  const options = {
    headless: !HEADFUL,
    timeout: 15_000,
    args: ['--disable-dev-shm-usage'],
  };
  if (explicitChromePath) {
    if (!existsSync(explicitChromePath)) {
      throw new Error(`Chrome executable does not exist: ${explicitChromePath}`);
    }
    options.executablePath = explicitChromePath;
  }
  return options;
}

function attr(value) {
  return String(value).replaceAll('\\', '\\\\').replaceAll('"', '\\"');
}

function escapeRegex(value) {
  return String(value).replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function menuPath(action, value) {
  if (action === 'SET_TIME') return ['Time'];
  if (action === 'SET_ENERGY') return ['Energy'];
  if (action === 'SET_DUE') return ['Due'];
  if (action === 'SET_SCHEDULE') return ['Schedule'];
  if (action === 'CONVERT_PROJECT') return ['Convert'];
  if (action === 'SET_PROJECT') return ['Move', 'Projects'];
  if (['TRASH', 'LOGBOOK'].includes(action)) return ['Move'];
  if (action === 'SET_LIST') {
    return ['State'];
  }
  return [];
}

async function visibleTaskTitles(page) {
  return page.$$eval('.task[data-task-title]', (rows) =>
    rows
      .filter((row) => row.offsetWidth || row.offsetHeight || row.getClientRects().length)
      .map((row) => row.dataset.taskTitle)
  );
}

async function waitForTask(page, title) {
  await page.waitForFunction((expected) => {
    return [...document.querySelectorAll('.task[data-task-title]')]
      .some((row) => row.dataset.taskTitle === expected && (row.offsetWidth || row.offsetHeight || row.getClientRects().length));
  }, title, { timeout: 5_000 });
  const row = page.locator(`.task[data-task-title="${attr(title)}"]`);
  await row.waitFor({ state: 'visible', timeout: 5_000 });
  assert.equal(await row.count(), 1, `Expected one visible task titled ${title}`);
  return row;
}

async function waitForNoTask(page, title) {
  await page.waitForFunction((expected) => {
    return ![...document.querySelectorAll('.task[data-task-title]')]
      .some((row) => row.dataset.taskTitle === expected && (row.offsetWidth || row.offsetHeight || row.getClientRects().length));
  }, title, { timeout: 5_000 });
}

async function clickNav(page, view) {
  const nav = page.locator(`.rail-link[data-view-link="${view}"], .footer-link[data-view-link="${view}"]`);
  assert.equal(await nav.count(), 1, `Expected one nav link for ${view}`);
  await nav.click();
  await page.waitForFunction((expected) => location.hash === `#${expected}`, view);
}

async function quickAdd(page, title) {
  await page.locator('#quick-add input[name="title"]').click();
  await page.locator('#quick-add input[name="title"]').fill(title);
  await page.keyboard.press('Enter');
  await waitForTask(page, title);
}

async function openMenu(page, title) {
  const row = await waitForTask(page, title);
  await row.hover();
  await row.locator('[data-action="MENU"]').click();
  await page.locator('.task-menu').waitFor({ state: 'visible', timeout: 5_000 });
}

async function clickMenuParent(page, label) {
  const parents = page.locator('.task-menu .menu-parent').filter({ hasText: new RegExp(`^${escapeRegex(label)}$`) });
  const count = await parents.count();
  assert.ok(count >= 1, `Expected submenu parent ${label}`);
  for (let index = 0; index < count; index += 1) {
    const parent = parents.nth(index);
    if (await parent.isVisible()) {
      await parent.click();
      return;
    }
  }
  throw new Error(`Submenu parent is not visible: ${label}`);
}

async function clickMenu(page, title, action, value) {
  await openMenu(page, title);
  for (const label of menuPath(action, value)) {
    await clickMenuParent(page, label);
  }
  const valueSelector = value === undefined ? '' : `[data-value="${attr(value)}"]`;
  const buttons = page.locator(`.task-menu [data-action="${action}"]${valueSelector}`);
  const count = await buttons.count();
  assert.ok(count >= 1, `Expected menu action ${action} ${value ?? ''}`.trim());
  for (let index = 0; index < count; index += 1) {
    const button = buttons.nth(index);
    if (await button.isVisible()) {
      await button.click();
      await page.locator('.task-menu').waitFor({ state: 'hidden', timeout: 5_000 });
      return;
    }
  }
  throw new Error(`Menu action is not visible: ${action} ${value ?? ''}`.trim());
}

async function dragTaskToTask(page, draggedTitle, targetTitle, position = 'before') {
  const dragged = await waitForTask(page, draggedTitle);
  const target = await waitForTask(page, targetTitle);
  const grip = await dragged.locator('.grip').boundingBox();
  const targetBox = await target.boundingBox();
  assert.ok(grip, `Missing drag grip for ${draggedTitle}`);
  assert.ok(targetBox, `Missing target box for ${targetTitle}`);
  const start = { x: grip.x + grip.width / 2, y: grip.y + grip.height / 2 };
  const endY = position === 'after' ? targetBox.y + targetBox.height - 3 : targetBox.y + 3;
  const end = { x: targetBox.x + 20, y: endY };
  await page.mouse.move(start.x, start.y);
  await page.mouse.down();
  await page.mouse.move(end.x, end.y, { steps: 8 });
  await page.mouse.up();
  await page.waitForFunction(({ draggedTitle: dragged, targetTitle: target, position: pos }) => {
    const titles = [...document.querySelectorAll('.task[data-task-title]')].map((row) => row.dataset.taskTitle);
    const draggedIndex = titles.indexOf(dragged);
    const targetIndex = titles.indexOf(target);
    if (draggedIndex < 0 || targetIndex < 0) return false;
    return pos === 'after' ? draggedIndex === targetIndex + 1 : draggedIndex === targetIndex - 1;
  }, { draggedTitle, targetTitle, position }, { timeout: 5_000 });
}

async function dragTaskToNav(page, title, view) {
  const row = await waitForTask(page, title);
  const nav = page.locator(`.rail-link[data-view-link="${view}"], .footer-link[data-view-link="${view}"]`);
  assert.equal(await nav.count(), 1, `Expected one nav target for ${view}`);
  const grip = await row.locator('.grip').boundingBox();
  const navBox = await nav.boundingBox();
  assert.ok(grip, `Missing drag grip for ${title}`);
  assert.ok(navBox, `Missing nav box for ${view}`);
  await page.mouse.move(grip.x + grip.width / 2, grip.y + grip.height / 2);
  await page.mouse.down();
  await page.mouse.move(navBox.x + navBox.width / 2, navBox.y + navBox.height / 2, { steps: 10 });
  await page.mouse.up();
  await waitForNoTask(page, title);
}

async function clickProject(page, title) {
  const nav = page.locator(`[data-project-link="${attr(title)}"]`);
  assert.equal(await nav.count(), 1, `Expected one project link for ${title}`);
  await nav.click();
  await page.waitForFunction((expected) => decodeURIComponent(location.hash).endsWith(`/project/${expected}`) || decodeURIComponent(location.hash).endsWith(`#project/${expected}`), title);
}

async function runBrowserSuite(baseUrl) {
  let browser;
  try {
    log('E2E: launch browser');
    browser = await chromium.launch(launchOptions());
  } catch (error) {
    if (/Executable doesn't exist/i.test(error.message)) {
      throw new Error(`Could not launch Playwright Chromium. Run "npm run test:e2e:install" once, then retry.\n${error.message}`);
    }
    throw new Error(`Could not launch Playwright Chromium. This machine may block standalone browser automation; retry in the in-app Browser sidebar or on a less restricted machine.\n${error.message}`);
  }
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  const browserErrors = [];
  page.on('pageerror', (error) => browserErrors.push(error.message));
  page.on('console', (message) => {
    if (message.type() === 'error') browserErrors.push(message.text());
  });

  const suffix = Date.now().toString(36);
  const alpha = slugTitle(`E2E ${suffix} alpha`);
  const alphaEdited = slugTitle(`E2E ${suffix} alpha edited`);
  const beta = slugTitle(`E2E ${suffix} beta`);
  const gamma = slugTitle(`E2E ${suffix} gamma`);
  const project = slugTitle(`E2E ${suffix} project`);
  const projectCopy = `${project} Copy`;

  try {
    log('E2E: load app shell');
    await page.goto(`${baseUrl}/#next`, { waitUntil: 'domcontentloaded' });
    await page.locator('#view-title').waitFor({ state: 'visible', timeout: 5_000 });
    assert.equal(await page.locator('#view-title').innerText(), 'Next');
    const topbar = await page.locator('.topbar').innerText();
    assert.match(topbar, /New Item/);
    assert.match(topbar, /Search/);
    assert.match(topbar, /Settings/);
    assert.doesNotMatch(topbar, /Refresh|Upgrade/);

    log('E2E: keyboard new item editor escape');
    await page.keyboard.press('n');
    await page.locator('[data-new-form]').waitFor({ state: 'visible', timeout: 5_000 });
    await page.keyboard.type(`draft ${suffix}`);
    await page.keyboard.press('Escape');
    await page.locator('[data-new-form]').waitFor({ state: 'hidden', timeout: 5_000 });
    assert.equal(await page.locator('#quick-add input[name="title"]').inputValue(), '');

    log('E2E: rapid create tasks');
    await quickAdd(page, alpha);
    await quickAdd(page, beta);
    await quickAdd(page, gamma);
    await quickAdd(page, project);

    log('E2E: area chips');
    await page.locator('.chip[data-area="other"]').click();
    await waitForTask(page, alpha);
    await page.locator('.chip[data-area="work"]').click();
    await waitForNoTask(page, alpha);
    await page.locator('.chip[data-area="all"]').click();
    await waitForTask(page, alpha);

    log('E2E: search and clear');
    await page.locator('#search').fill(beta);
    await waitForTask(page, beta);
    await waitForNoTask(page, alpha);
    await page.locator('#search').fill(`no result ${suffix}`);
    await page.locator('[data-clear-search]').click();
    await waitForTask(page, alpha);

    log('E2E: edit task fields');
    const alphaRow = await waitForTask(page, alpha);
    await alphaRow.dblclick();
    const editForm = page.locator(`form.task[data-task-title="${attr(alpha)}"]`);
    await editForm.waitFor({ state: 'visible', timeout: 5_000 });
    await editForm.locator('input[name="title"]').fill(alphaEdited);
    await editForm.locator('input[name="tags"]').fill(`e2e, ${suffix}`);
    await editForm.locator('textarea[name="notes"]').fill('Edited from browser automation.');
    await editForm.locator('select[name="area"]').selectOption('work');
    await editForm.locator('select[name="effort"]').selectOption('15m');
    await editForm.locator('select[name="energy"]').selectOption('medium');
    await editForm.locator('button[type="submit"]').click();
    await waitForTask(page, alphaEdited);
    await waitForNoTask(page, alpha);

    log('E2E: focus star and focus view');
    const editedRow = await waitForTask(page, alphaEdited);
    await editedRow.locator('[data-action="FOCUS"]').click();
    await clickNav(page, 'focus');
    await waitForTask(page, alphaEdited);
    await clickNav(page, 'next');

    log('E2E: menu field changes');
    await clickMenu(page, alphaEdited, 'SET_TIME', '10m');
    await waitForTask(page, alphaEdited);
    assert.match(await (await waitForTask(page, alphaEdited)).innerText(), /10m/);
    await clickMenu(page, alphaEdited, 'SET_ENERGY', 'high');
    assert.match(await (await waitForTask(page, alphaEdited)).innerText(), /high/);
    const today = new Date().toISOString().slice(0, 10);
    await clickMenu(page, alphaEdited, 'SET_DUE', today);
    assert.match(await (await waitForTask(page, alphaEdited)).innerText(), /Due/);

    log('E2E: scheduled move and unschedule');
    const tomorrow = new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString().slice(0, 10);
    await clickMenu(page, alphaEdited, 'SET_SCHEDULE', tomorrow);
    await waitForNoTask(page, alphaEdited);
    await clickNav(page, 'scheduled');
    await waitForTask(page, alphaEdited);
    await clickMenu(page, alphaEdited, 'SET_SCHEDULE', '');
    await waitForNoTask(page, alphaEdited);
    await clickNav(page, 'next');
    await waitForTask(page, alphaEdited);

    log('E2E: drag reorder');
    await dragTaskToTask(page, gamma, beta, 'before');
    const titlesAfterDrag = await visibleTaskTitles(page);
    assert.ok(titlesAfterDrag.indexOf(gamma) < titlesAfterDrag.indexOf(beta), 'Expected dragged task before target task');

    log('E2E: drag task to Work area nav');
    await page.locator('.chip[data-area="other"]').click();
    await waitForTask(page, gamma);
    await dragTaskToNav(page, gamma, 'work');
    await clickNav(page, 'work');
    await waitForTask(page, gamma);
    await clickMenu(page, gamma, 'SET_LIST', 'next');
    await clickNav(page, 'next');
    await waitForTask(page, gamma);

    log('E2E: copy and convert project');
    await clickMenu(page, project, 'COPY');
    await waitForTask(page, projectCopy);
    await clickMenu(page, project, 'CONVERT_PROJECT');
    assert.match(await (await waitForTask(page, project)).innerText(), /PROJ/);
    await clickMenu(page, gamma, 'SET_PROJECT', project);
    await clickNav(page, 'projects');
    await page.locator('.project-card').filter({ hasText: project }).waitFor({ state: 'visible', timeout: 5_000 });
    await clickProject(page, project);
    assert.match(await page.locator('.project-summary-card').innerText(), new RegExp(escapeRegex(project)));
    await waitForTask(page, gamma);
    await clickNav(page, 'next');

    log('E2E: done and logbook');
    const betaRow = await waitForTask(page, beta);
    await betaRow.locator('[data-action="DONE"]').click();
    await waitForNoTask(page, beta);
    await clickNav(page, 'logbook');
    await waitForTask(page, beta);

    log('E2E: trash restore delete');
    await clickNav(page, 'next');
    await clickMenu(page, alphaEdited, 'TRASH');
    await waitForNoTask(page, alphaEdited);
    await clickNav(page, 'trash');
    await waitForTask(page, alphaEdited);
    const trashRow = await waitForTask(page, alphaEdited);
    await trashRow.hover();
    await trashRow.locator('[data-action="RESTORE"]').click();
    await waitForNoTask(page, alphaEdited);
    await clickNav(page, 'next');
    await waitForTask(page, alphaEdited);
    await clickMenu(page, alphaEdited, 'TRASH');
    await clickNav(page, 'trash');
    const deleteRow = await waitForTask(page, alphaEdited);
    await deleteRow.hover();
    await deleteRow.locator('[data-action="DELETE"]').click();
    await waitForNoTask(page, alphaEdited);

    log('E2E: settings export');
    await page.locator('#settings').click();
    await page.locator('#settings-panel').waitFor({ state: 'visible', timeout: 5_000 });
    await page.locator('#export-org').click();
    await page.waitForFunction(() => /Exported /.test(document.querySelector('#export-status')?.textContent || ''), null, { timeout: 5_000 });

    log('E2E: keyboard nav and shortcuts');
    await page.mouse.click(500, 82);
    await page.keyboard.press('/');
    assert.equal(await page.evaluate(() => document.activeElement?.id), 'search');
    await page.locator('#search').fill('');
    await page.mouse.click(500, 82);
    let shortcutText = '';
    page.once('dialog', async (dialog) => {
      shortcutText = dialog.message();
      await dialog.dismiss();
    });
    await page.keyboard.press('k');
    await page.waitForTimeout(100);
    assert.match(shortcutText, /Create: n/);
    assert.doesNotMatch(shortcutText, /Refresh/);
    await page.keyboard.press('5');
    await page.waitForFunction(() => location.hash === '#review');
    assert.equal(await page.locator('#view-title').innerText(), 'Review');
    await page.keyboard.press('3');
    await page.waitForFunction(() => location.hash === '#next');

    assert.deepEqual(browserErrors, [], `Browser console/page errors:\n${browserErrors.join('\n')}`);
  } finally {
    await browser.close();
  }
}

async function main() {
  const tmp = await mkdtemp(path.join(tmpdir(), 'gtd-web-e2e-'));
  const port = await freePort();
  const baseUrl = `http://127.0.0.1:${port}`;
  const currentFile = path.join(tmp, 'current.org');
  const archiveFile = path.join(tmp, 'archive.org');
  const dbFile = path.join(tmp, 'gtd.sqlite');
  const exportFile = path.join(tmp, 'export.org');

  await writeFile(currentFile, '* Inbox\n', 'utf8');
  await writeFile(archiveFile, '* Archive\n', 'utf8');

  const server = spawn(process.execPath, ['--disable-warning=ExperimentalWarning', 'server.mjs'], {
    cwd: ROOT,
    env: {
      ...process.env,
      GTD_PORT: String(port),
      GTD_HOST: '127.0.0.1',
      GTD_CURRENT_FILE: currentFile,
      GTD_ARCHIVE_FILE: archiveFile,
      GTD_DB_FILE: dbFile,
      GTD_EXPORT_FILE: exportFile,
      GTD_AUTO_EXPORT: '0',
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });

  let serverOutput = '';
  server.stdout.on('data', (chunk) => { serverOutput += chunk; });
  server.stderr.on('data', (chunk) => { serverOutput += chunk; });

  try {
    await waitForServer(baseUrl, server);
    await runBrowserSuite(baseUrl);
    log('E2E: passed');
  } catch (error) {
    if (serverOutput) {
      process.stderr.write('\n--- server output ---\n');
      process.stderr.write(serverOutput);
      process.stderr.write('--- end server output ---\n');
    }
    throw error;
  } finally {
    server.kill('SIGTERM');
    await new Promise((resolve) => {
      if (server.exitCode !== null) return resolve();
      server.once('exit', resolve);
      setTimeout(resolve, 1_000);
    });
    await rm(tmp, { recursive: true, force: true });
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
