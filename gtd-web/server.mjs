import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { createReadStream } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createGtdStore } from './src/db.js';
import { expandHome } from './src/org.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PUBLIC_DIR = path.join(__dirname, 'public');
const PORT = Number(process.env.GTD_PORT || 8787);
const HOST = process.env.GTD_HOST || '127.0.0.1';

const config = {
  currentFile: process.env.GTD_CURRENT_FILE || '~/org/gtd/current.org',
  archiveFile: process.env.GTD_ARCHIVE_FILE || '~/org/gtd/archive.org',
  dbFile: process.env.GTD_DB_FILE || path.join(__dirname, 'data/gtd.sqlite'),
  exportFile: process.env.GTD_EXPORT_FILE || '~/org/gtd/gtd-web-export.org',
  autoExport: process.env.GTD_AUTO_EXPORT !== '0',
  days: Number(process.env.GTD_REVIEW_DAYS || 7),
  staleDays: Number(process.env.GTD_STALE_DAYS || 14),
};

const store = await createGtdStore(config);

const MIME = new Map([
  ['.html', 'text/html; charset=utf-8'],
  ['.css', 'text/css; charset=utf-8'],
  ['.js', 'text/javascript; charset=utf-8'],
  ['.json', 'application/json; charset=utf-8'],
  ['.svg', 'image/svg+xml'],
]);

function sendJson(res, status, body) {
  const payload = JSON.stringify(body);
  res.writeHead(status, {
    'content-type': 'application/json; charset=utf-8',
    'cache-control': 'no-store',
    'content-length': Buffer.byteLength(payload),
  });
  res.end(payload);
}

async function withAutoExport(result) {
  if (!config.autoExport) return result;
  try {
    return { ...result, export: await store.writeOrgExport() };
  } catch (error) {
    return { ...result, export: { ok: false, error: error.message } };
  }
}

async function sendMutationJson(res, status, result) {
  sendJson(res, status, await withAutoExport(result));
}

function readBody(req) {
  return new Promise((resolve, reject) => {
    let data = '';
    req.setEncoding('utf8');
    req.on('data', (chunk) => {
      data += chunk;
      if (data.length > 1024 * 1024) {
        reject(new Error('Request body too large'));
        req.destroy();
      }
    });
    req.on('end', () => {
      try {
        resolve(data ? JSON.parse(data) : {});
      } catch {
        reject(new Error('Invalid JSON'));
      }
    });
    req.on('error', reject);
  });
}

function limitText(value = '', max = 100_000) {
  const text = String(value || '').replace(/\s+\n/g, '\n').replace(/[ \t]{2,}/g, ' ').trim();
  return text.length > max ? `${text.slice(0, max)}\n\n[truncated]` : text;
}

function decodeEntities(value = '') {
  return String(value)
    .replaceAll('&amp;', '&')
    .replaceAll('&lt;', '<')
    .replaceAll('&gt;', '>')
    .replaceAll('&quot;', '"')
    .replaceAll('&#39;', "'");
}

function stripHtml(value = '') {
  return decodeEntities(String(value)
    .replace(/<script[\s\S]*?<\/script>/gi, ' ')
    .replace(/<style[\s\S]*?<\/style>/gi, ' ')
    .replace(/<noscript[\s\S]*?<\/noscript>/gi, ' ')
    .replace(/<[^>]+>/g, ' ')
    .replace(/\s+/g, ' '));
}

function sourceTypeForUrl(rawUrl, explicitType = '') {
  const type = String(explicitType || '').trim().toLowerCase();
  if (['twitter', 'article', 'youtube', 'pdf', 'github', 'doc', 'other'].includes(type)) return type;
  try {
    const url = new URL(rawUrl);
    const host = url.hostname.replace(/^www\./, '').toLowerCase();
    if (host === 'x.com' || host === 'twitter.com' || host.endsWith('.twitter.com')) return 'twitter';
    if (host === 'youtube.com' || host === 'youtu.be' || host.endsWith('.youtube.com')) return 'youtube';
    if (host === 'github.com') return 'github';
    if (url.pathname.toLowerCase().endsWith('.pdf')) return 'pdf';
    if (url.pathname.toLowerCase().match(/\.(md|txt|org)$/)) return 'doc';
    return 'article';
  } catch {
    return 'other';
  }
}

function titleFromUrl(rawUrl) {
  try {
    const url = new URL(rawUrl);
    const tail = decodeURIComponent(url.pathname.split('/').filter(Boolean).pop() || url.hostname);
    return tail.replace(/[-_]+/g, ' ').trim() || url.hostname;
  } catch {
    return rawUrl;
  }
}

function twitterStatusId(rawUrl) {
  try {
    const url = new URL(rawUrl);
    const match = url.pathname.match(/\/status\/(\d+)/);
    return match?.[1] || '';
  } catch {
    return '';
  }
}

function deepString(value, keys) {
  if (!value || typeof value !== 'object') return '';
  for (const key of keys) {
    if (typeof value[key] === 'string' && value[key].trim()) return value[key].trim();
  }
  for (const child of Object.values(value)) {
    const found = deepString(child, keys);
    if (found) return found;
  }
  return '';
}

async function fetchWithTimeout(url, options = {}) {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), options.timeout || 10_000);
  try {
    return await fetch(url, {
      ...options,
      signal: controller.signal,
      headers: {
        'user-agent': 'gtd-source-fetcher/1.0',
        accept: 'text/html,application/json,text/plain;q=0.9,*/*;q=0.8',
        ...(options.headers || {}),
      },
    });
  } finally {
    clearTimeout(timeout);
  }
}

async function fetchSourceSnapshot(input) {
  const rawUrl = String(input.url || '').trim();
  const url = new URL(rawUrl);
  const type = sourceTypeForUrl(url.href, input.type);
  const base = {
    ...input,
    url: url.href,
    type,
    title: String(input.title || titleFromUrl(url.href)).trim(),
    status: input.status || 'unread',
  };
  if (input.fetch === false || input.rawText) return base;
  try {
    if (type === 'twitter') {
      const statusId = twitterStatusId(url.href);
      if (statusId) {
        const response = await fetchWithTimeout(`https://api.fxtwitter.com/2/status/${statusId}`, {
          headers: { accept: 'application/json' },
        });
        if (response.ok) {
          const data = await response.json();
          const text = deepString(data, ['article_text', 'text', 'full_text', 'description']);
          const title = deepString(data, ['title']) || (text ? text.slice(0, 90) : base.title);
          const author = deepString(data, ['screen_name', 'name', 'author_name']);
          return {
            ...base,
            title,
            author: input.author || author,
            rawText: limitText(text || JSON.stringify(data, null, 2)),
            summary: input.summary || limitText(text, 700),
            fetchedAt: new Date().toISOString(),
          };
        }
      }
    }
    if (type === 'youtube') {
      const response = await fetchWithTimeout(`https://www.youtube.com/oembed?format=json&url=${encodeURIComponent(url.href)}`, {
        headers: { accept: 'application/json' },
      });
      if (response.ok) {
        const data = await response.json();
        return {
          ...base,
          title: input.title || data.title || base.title,
          author: input.author || data.author_name || '',
          summary: input.summary || 'Video source captured. Transcript is not fetched yet.',
          rawText: input.rawText || '',
          fetchedAt: new Date().toISOString(),
        };
      }
    }
    if (type === 'pdf') {
      return {
        ...base,
        summary: input.summary || 'PDF source captured. Text extraction is not enabled yet.',
        rawText: input.rawText || '',
        fetchedAt: new Date().toISOString(),
      };
    }
    const response = await fetchWithTimeout(url.href);
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const contentType = response.headers.get('content-type') || '';
    const text = await response.text();
    if (contentType.includes('text/html') || /<html[\s>]/i.test(text)) {
      const title = decodeEntities(text.match(/<title[^>]*>([\s\S]*?)<\/title>/i)?.[1]?.replace(/\s+/g, ' ').trim() || base.title);
      const rawText = limitText(stripHtml(text));
      return {
        ...base,
        title: input.title || title,
        rawText,
        summary: input.summary || limitText(rawText, 700),
        fetchedAt: new Date().toISOString(),
      };
    }
    const rawText = limitText(text);
    return {
      ...base,
      rawText,
      summary: input.summary || limitText(rawText, 700),
      fetchedAt: new Date().toISOString(),
    };
  } catch (error) {
    return {
      ...base,
      summary: input.summary || `Captured URL. Fetch failed: ${error.message}`,
      rawText: input.rawText || '',
    };
  }
}

async function serveStatic(req, res, pathname) {
  let target = pathname === '/' ? '/index.html' : pathname;
  target = decodeURIComponent(target);
  const file = path.normalize(path.join(PUBLIC_DIR, target));
  if (!file.startsWith(PUBLIC_DIR)) {
    res.writeHead(403);
    res.end('Forbidden');
    return;
  }
  try {
    await readFile(file);
    res.writeHead(200, {
      'content-type': MIME.get(path.extname(file)) || 'application/octet-stream',
      'cache-control': 'no-store',
    });
    createReadStream(file).pipe(res);
  } catch {
    res.writeHead(404);
    res.end('Not found');
  }
}

async function route(req, res) {
  const url = new URL(req.url, `http://${req.headers.host || `${HOST}:${PORT}`}`);
  try {
    if (req.method === 'GET' && url.pathname === '/api/state') {
      sendJson(res, 200, store.readState());
      return;
    }

    if (req.method === 'POST' && url.pathname === '/api/tasks') {
      const body = await readBody(req);
      const result = store.addTask(body);
      await sendMutationJson(res, 201, result);
      return;
    }

    if (req.method === 'POST' && url.pathname === '/api/sources') {
      const body = await readBody(req);
      const result = store.addSource(await fetchSourceSnapshot(body));
      sendJson(res, 201, result);
      return;
    }

    const sourceMatch = url.pathname.match(/^\/api\/sources\/([^/]+)$/);
    if (req.method === 'PATCH' && sourceMatch) {
      const body = await readBody(req);
      sendJson(res, 200, store.updateSource(sourceMatch[1], body));
      return;
    }

    if (req.method === 'DELETE' && sourceMatch) {
      sendJson(res, 200, store.deleteSource(sourceMatch[1]));
      return;
    }

    const sourceTaskMatch = url.pathname.match(/^\/api\/sources\/([^/]+)\/tasks$/);
    if (req.method === 'POST' && sourceTaskMatch) {
      const body = await readBody(req);
      await sendMutationJson(res, 201, store.createTaskFromSource(sourceTaskMatch[1], body));
      return;
    }

    if (req.method === 'PATCH' && url.pathname === '/api/tasks/reorder') {
      const body = await readBody(req);
      await sendMutationJson(res, 200, store.reorderTasks(body.ids));
      return;
    }

    const updateMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)$/);
    if (req.method === 'PATCH' && updateMatch) {
      const body = await readBody(req);
      await sendMutationJson(res, 200, store.updateTask(updateMatch[1], body));
      return;
    }

    if (req.method === 'DELETE' && updateMatch) {
      await sendMutationJson(res, 200, store.deleteTask(updateMatch[1]));
      return;
    }

    const stateMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/state$/);
    if (req.method === 'PATCH' && stateMatch) {
      const body = await readBody(req);
      const result = store.updateTaskState(stateMatch[1], body.todo);
      await sendMutationJson(res, 200, result);
      return;
    }

    const focusMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/focus$/);
    if (req.method === 'PATCH' && focusMatch) {
      const body = await readBody(req);
      await sendMutationJson(res, 200, store.setTaskFocus(focusMatch[1], body.focus));
      return;
    }

    const logbookMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/logbook$/);
    if (req.method === 'POST' && logbookMatch) {
      await sendMutationJson(res, 200, store.moveToLogbook(logbookMatch[1]));
      return;
    }

    const copyMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/copy$/);
    if (req.method === 'POST' && copyMatch) {
      await sendMutationJson(res, 200, store.copyTask(copyMatch[1]));
      return;
    }

    const convertProjectMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/convert\/project$/);
    if (req.method === 'POST' && convertProjectMatch) {
      await sendMutationJson(res, 200, store.convertToProject(convertProjectMatch[1]));
      return;
    }

    const trashMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/trash$/);
    if (req.method === 'POST' && trashMatch) {
      await sendMutationJson(res, 200, store.moveToTrash(trashMatch[1]));
      return;
    }

    const restoreMatch = url.pathname.match(/^\/api\/tasks\/([^/]+)\/restore$/);
    if (req.method === 'POST' && restoreMatch) {
      await sendMutationJson(res, 200, store.restoreTask(restoreMatch[1]));
      return;
    }

    if (req.method === 'GET' && url.pathname === '/api/export/org') {
      const text = store.exportOrgText();
      res.writeHead(200, {
        'content-type': 'text/plain; charset=utf-8',
        'cache-control': 'no-store',
        'content-length': Buffer.byteLength(text),
      });
      res.end(text);
      return;
    }

    if (req.method === 'POST' && url.pathname === '/api/export/org') {
      sendJson(res, 200, await store.writeOrgExport());
      return;
    }

    if (req.method === 'GET' || req.method === 'HEAD') {
      await serveStatic(req, res, url.pathname);
      return;
    }

    sendJson(res, 405, { ok: false, error: 'Method not allowed' });
  } catch (error) {
    sendJson(res, 400, { ok: false, error: error.message });
  }
}

const server = createServer((req, res) => {
  route(req, res);
});

server.listen(PORT, HOST, () => {
  console.log(`GTD web app: http://${HOST}:${PORT}`);
  console.log(`SQLite DB: ${store.dbFile}`);
  console.log(`Org seed: ${expandHome(config.currentFile)}`);
  console.log(`Org export: ${store.exportFile}${config.autoExport ? ' (auto)' : ''}`);
});
