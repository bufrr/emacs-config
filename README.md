# My Doom Emacs Configuration

This is my personal Doom Emacs configuration with a focus on GTD (Getting Things Done) workflow and productivity, plus a complete blogging system.

## Features

- 🚀 **Simple 2-File GTD**: Clean Getting Things Done system with current work + archive
- 📊 **Progress Tracking**: Automatic progress indicators for projects and subtasks
- 📝 **Blog System**: Full org-mode based static blog generator with custom themes
- ⚡ **Smart Keybindings**: Minimal but powerful GTD shortcuts
- ✨ **Hierarchical Tasks**: Full support for nested tasks with completion tracking

## Installation

1. Install [Doom Emacs](https://github.com/doomemacs/doomemacs) first
2. Clone this repository:
   ```bash
   git clone https://github.com/YOUR_USERNAME/doom-config.git ~/.config/doom
   ```
3. Run `doom sync` to install packages and compile
4. Restart Emacs

## Keybindings

### GTD Keybindings

| Key | Description |
|-----|-------------|
| `SPC g c` | Capture new item (task/project/idea) |
| `SPC g a` | View agenda |
| `SPC g A` | Archive completed subtree |
| `SPC g w` | Open current work file |
| `SPC g v` | View archive |
| `SPC g G` | Open GTD dashboard |
| `SPC g h` | Open AgentDeck |
| `SPC g q` | Stop AgentDeck |
| `SPC g t` | Add and classify raw task |
| `SPC g d` | Add deep work task |
| `SPC g s` | Add shallow task |
| `SPC g l` | Add learning task |
| `SPC g x` | Complete matching task |
| `SPC g C` | Cleanup GTD headings/statistics |
| `SPC g X` | Archive completed GTD entries after confirmation |
| `SPC g b` | Insert checkbox |

### Blog Keybindings

| Key | Description |
|-----|-------------|
| `SPC B n` | Create new blog post |
| `SPC B p` | Publish blog |

## Structure

- `config.el` - Main configuration file
- `init.el` - Doom modules declaration
- `packages.el` - Additional package declarations
- `CLAUDE.md` - AI assistant instructions for this codebase

## File Organization

### GTD Files (2-File System)
The GTD system uses just two files in `~/org/gtd/`:
- `current.org` - All active work (projects, tasks, ideas)
- `archive.org` - Completed and archived items
- `README.md` - Complete GTD documentation
- `quick-reference.org` - Quick key reference

`current.org` is normalized around these top-level headings:

```org
* Inbox
* Tasks
* Projects
* Work
* Learning
* Ideas
```

### Blog Files
The blog system uses this structure in `~/blog/`:
- `posts/` - Your blog posts in org format
- `public/` - Generated HTML output
- `static/` - CSS, JS, and other assets
- `templates/` - Post templates

## Customization

Feel free to modify the configuration to suit your needs. Key areas:

### GTD Customization
- Task states: TODO, PROJ (for projects), DONE, CANCELLED
- Five capture templates: Task (t), Deep work (d), Shallow task (s), Project (p), Idea (i)
- Assistant-friendly task intake, duplicate detection, completion, cleanup, and archive helpers
- Automatic progress tracking with [/] and [%] indicators
- Archive location set to archive.org

### Blog Customization
- Blog metadata in `blog-*` variables
- HTML templates in `blog/preamble` and `blog/postamble`
- CSS styles in `static/css/blog.css`
- Publishing settings in `org-publish-project-alist`

## Usage Tips

### GTD Workflow (Simple 2-File System)
1. **Capture**: `SPC g c` then choose template:
   - `t` for tasks
   - `d` for deep work
   - `s` for shallow tasks
   - `p` for projects (with progress tracking)
   - `i` for ideas
2. **Work**: `SPC g w` to view current work file
3. **Assistant intake**: `SPC g t` adds and classifies a raw task phrase
4. **Complete**: Mark items DONE with `C-c C-t d` or `SPC g x`
5. **Daily UI**: `SPC g h` opens AgentDeck for Next, Inbox, Waiting, Scheduled, Someday, and source views
6. **Review**: `SPC g G` for the GTD dashboard, `SPC g a` for agenda, or `SPC g v` for archive
7. **Archive**: `SPC g A` archives the current subtree; `SPC g X` archives completed GTD entries after confirmation

### AgentDeck

AgentDeck now lives in a separate repository:

```sh
cd ~/agentdeck
```

AgentDeck uses SQLite as its primary store. Org files are compatibility boundaries: it can import `~/org/gtd/current.org` and `~/org/gtd/archive.org` when the SQLite database is empty, and it can export an Org backup.

Run it manually with:

```sh
cd ~/agentdeck
npm start
```

Then open `http://127.0.0.1:8787`.

### Blog Workflow
1. Create new post with `SPC B n`
2. Write in org-mode with all its features
3. Publish with `SPC B p`
4. Generated site will be in `~/blog/public/`

---

## Org Mode & GTD Guide

This section provides a comprehensive guide to using org-mode with the GTD (Getting Things Done) methodology in this configuration.

### Task States

This config uses four task states:

| State | Shortcut | Description |
|-------|----------|-------------|
| `TODO` | `t` | Active tasks that need to be done |
| `PROJ` | `p` | Projects containing multiple subtasks |
| `DONE` | `d` | Completed tasks |
| `CANCELLED` | `c` | Tasks that are no longer relevant |

**Change task state**: Place cursor on a heading and press `C-c C-t`, then the shortcut letter.

### Creating Tasks and Projects

#### Simple Task
```org
* Tasks
** TODO Buy groceries
   [2024-12-16 Mon]
```

#### Project with Subtasks (TODO children)
```org
* Projects
** PROJ Website Redesign [33%]
   [2024-12-16 Mon]
*** TODO Design mockups
*** DONE Set up hosting
*** TODO Implement frontend
```

#### Project with Checkboxes
```org
* Projects
** PROJ Learn Emacs [1/3]
   [2024-12-16 Mon]
   - [X] Install Doom Emacs
   - [ ] Learn keybindings
   - [ ] Customize config
```

### Progress Tracking

This configuration includes automatic progress tracking for projects.

#### Percentage Cookies `[%]`
Shows completion percentage. Add `[%]` to any heading:
```org
** PROJ My Project [50%]
*** DONE Task 1
*** TODO Task 2
```

#### Fraction Cookies `[/]`
Shows completed/total count. Add `[/]` to any heading:
```org
** PROJ My Project [1/2]
*** DONE Task 1
*** TODO Task 2
```

#### Checkbox Statistics
For checkbox lists, use `[/]` or `[%]`:
```org
** TODO Shopping [2/4]
   - [X] Milk
   - [X] Bread
   - [ ] Eggs
   - [ ] Butter
```

**Auto-update statistics**: Press `C-c C-c` on a checkbox, or use `SPC g u` to force update all statistics.

### Automatic Features

This configuration includes several automatic behaviors:

1. **Auto-complete parent**: When all subtasks/checkboxes are done, the parent automatically becomes `DONE`
2. **Auto-reopen parent**: If a child is reopened, the parent reverts to `TODO`/`PROJ`
3. **CLOSED timestamp**: When marking as `DONE`, a `CLOSED:` timestamp is automatically added
4. **Statistics update**: Checkbox counts and percentages update automatically when toggling

### Capture Templates

Use `SPC g c` to capture new items:

| Key | Template | Description |
|-----|----------|-------------|
| `t` | Task | Creates a new TODO under "Tasks" with `:Created:` and `:Source:` |
| `d` | Deep work | Creates a focused TODO under "Tasks" with `:deep:` |
| `s` | Shallow task | Creates a quick TODO under "Tasks" with `:shallow:` |
| `p` | Project | Creates a new PROJ with `[0%]` cookie, metadata, and first subtask |
| `i` | Idea | Creates a plain entry under "Ideas" with metadata |

Captured entries include:

```org
:PROPERTIES:
:Created: [2026-04-26 Sun 15:10]
:Source: manual
:END:
```

Assistant-created entries use `:Source: codex`.

### Assistant Task Intake

Use `SPC g t` or tell the assistant a raw task phrase. The helper classifies the task, adds tags, estimates effort, and prevents open duplicates.

Example input:

```text
hyperliquid api check
```

Example result:

```org
** TODO Hyperliquid API Check                                      :deep:work:
   :PROPERTIES:
   :Effort: 1:00
   :Created: [2026-04-26 Sun 15:10]
   :Source: codex
   :END:
```

Completion phrases such as `finished hyperliquid api check` can be handled with `SPC g x` or by telling the assistant. Ambiguous matches ask for confirmation instead of guessing.

### Practical Workflow Examples

#### Daily Task Management
```org
* Tasks
** TODO Reply to John's email
** TODO Review PR #123
** DONE Fix login bug
   CLOSED: [2024-12-16 Mon 14:30]
```

#### Project Planning
```org
* Projects
** PROJ Q1 Marketing Campaign [25%]
   [2024-12-15 Sun]
*** DONE Define target audience
*** TODO Create content calendar
*** TODO Design social media assets
*** TODO Launch campaign
```

#### Mixed Approach (Subtasks + Checkboxes)
```org
** PROJ App Release [50%]
*** DONE Development [4/4]
    - [X] Core features
    - [X] API integration
    - [X] Testing
    - [X] Bug fixes
*** TODO Deployment [0/3]
    - [ ] Stage deployment
    - [ ] Production deployment
    - [ ] Monitor metrics
```

### Essential Org-Mode Keys

| Key | Action |
|-----|--------|
| `SPC g c` | Capture with heading normalization |
| `SPC g t` | Add and classify a raw task |
| `SPC g d` | Add deep work task |
| `SPC g s` | Add shallow task |
| `SPC g l` | Add learning task |
| `SPC g x` | Complete matching task |
| `SPC g C` | Cleanup GTD headings/statistics |
| `SPC g X` | Archive completed GTD entries after confirmation |
| `SPC g G` | Open GTD dashboard |
| `C-c C-t` | Cycle TODO state |
| `C-c C-c` | Toggle checkbox / Update statistics |
| `TAB` | Fold/unfold heading |
| `S-TAB` | Fold/unfold all headings |
| `M-RET` | Insert new heading at same level |
| `M-S-RET` | Insert new TODO at same level |
| `M-LEFT/RIGHT` | Promote/demote heading |
| `M-UP/DOWN` | Move subtree up/down |
| `C-c C-s` | Schedule task |
| `C-c C-d` | Set deadline |
| `C-c [` | Add file to agenda |
| `C-c ]` | Remove file from agenda |

### Agenda Views

Access agenda with `SPC g a`, then choose:

| Key | View |
|-----|------|
| `a` | Agenda (week view with scheduled/deadline items) |
| `t` | All TODOs |
| `m` | Match by tags |
| `s` | Search |
| `d` | Deep Work Queue |
| `S` | Shallow Work Queue |
| `w` | Work overview |
| `g` | GTD dashboard |

### Tips for Effective GTD

1. **Capture everything**: Use `SPC g c` liberally — capture ideas immediately
2. **Keep projects actionable**: Every PROJ should have at least one TODO subtask
3. **Regular review**: Check `SPC g a` daily for scheduled items and deadlines
4. **Archive completed work**: Use `SPC g X` after review; `SPC g A` archives only the current subtree
5. **Use tags**: Add `:work:` or `:personal:` tags to organize by context
6. **Schedule wisely**: Use `C-c C-s` for things you plan to do on specific days

### Refiling

Move items between sections with:
- `C-c C-w` — Refile to another location in the current file (up to 3 levels deep)

### Adding Tags

Add tags to organize tasks by context:
```org
** TODO Call dentist                                         :personal:health:
** TODO Review quarterly report                              :work:urgent:
```

Press `C-c C-q` on a heading to add/edit tags.

### Recurring / Daily Tasks

For tasks that repeat on a schedule, use **repeaters** in timestamps.

#### Setting Up a Daily Task
```org
** TODO Review inbox
   SCHEDULED: <2024-12-16 Mon +1d>
```

The `+1d` means "repeat every 1 day".

#### How to Create
1. Create a TODO: `SPC g c t`
2. Add schedule: `C-c C-s`
3. Type the date, then add the repeater (e.g., `+1d`)

#### Repeater Syntax

| Repeater | Meaning |
|----------|---------|
| `+1d` | Every day |
| `+1w` | Every week |
| `+2w` | Every 2 weeks |
| `+1m` | Every month |
| `+1y` | Every year |

#### Repeater Types

| Type | Behavior |
|------|----------|
| `+1d` | Shifts from original SCHEDULED date |
| `++1d` | Shifts from TODAY (catches up if overdue) |
| `.+1d` | Shifts from when you mark DONE |

**Best for daily habits**: Use `.+1d` — it reschedules based on completion date.

#### Example Daily Tasks
```org
* Habits
** TODO Morning exercise                                        :habit:
   SCHEDULED: <2024-12-16 Mon .+1d>
** TODO Review inbox
   SCHEDULED: <2024-12-16 Mon +1d>
** TODO Journal
   SCHEDULED: <2024-12-16 Mon .+1d>
```

#### What Happens on Completion
When you mark a recurring task `DONE` (`C-c C-t d`), org-mode automatically:
1. Resets it back to `TODO`
2. Shifts the scheduled date forward
3. Logs the completion in a `:LOGBOOK:` drawer

## License

This configuration is available under the MIT License.
