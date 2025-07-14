# My Doom Emacs Configuration

This is my personal Doom Emacs configuration with a focus on GTD (Getting Things Done) workflow and productivity, plus a complete blogging system.

## Features

- 🚀 **GTD Implementation**: Complete Getting Things Done system with interactive inbox processing
- 📝 **Blog System**: Full org-mode based static blog generator with custom themes
- ⚡ **Optimized Keybindings**: Efficient shortcuts for common tasks
- 🎯 **Context-based Organization**: Tasks organized by context (@home, @work, etc.)
- 📅 **Weekly Review System**: Automated reminders and project tracking
- 🔋 **Energy Management**: Tag tasks by energy level required

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
| `SPC g x` | Quick capture |
| `SPC g I` | Process inbox interactively |
| `SPC g r` | GTD review dashboard |
| `SPC g n` | Next actions by context |
| `SPC g p` | Projects overview |
| `SPC g w` | Weekly review |
| `SPC g a` | Archive done tasks |
| `SPC g s` | Find stale projects |

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

### GTD Files
The GTD system creates these files in `~/org/gtd/`:
- `inbox.org` - Capture destination
- `gtd.org` - Main GTD file (Projects, Next Actions, etc.)
- `calendar.org` - Scheduled appointments
- `archive.org` - Archived completed tasks (auto-created)

### Blog Files
The blog system uses this structure in `~/blog/`:
- `posts/` - Your blog posts in org format
- `public/` - Generated HTML output
- `static/` - CSS, JS, and other assets
- `templates/` - Post templates

## Customization

Feel free to modify the configuration to suit your needs. Key areas:

### GTD Customization
- GTD contexts in `org-tag-alist`
- Capture templates in `org-capture-templates`
- Custom agenda views in `org-agenda-custom-commands`
- Energy levels and effort estimates

### Blog Customization
- Blog metadata in `blog-*` variables
- HTML templates in `blog/preamble` and `blog/postamble`
- CSS styles in `static/css/blog.css`
- Publishing settings in `org-publish-project-alist`

## Usage Tips

### GTD Workflow
1. Capture everything with `SPC g x i`
2. Process inbox daily with `SPC g I`
3. Review projects weekly with `SPC g w`
4. Archive old tasks with `SPC g a`

### Blog Workflow
1. Create new post with `SPC B n`
2. Write in org-mode with all its features
3. Publish with `SPC B p`
4. Generated site will be in `~/blog/public/`

## License

This configuration is available under the MIT License.