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
- Three capture templates: Task (t), Project (p), Idea (i)
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
   - `p` for projects (with progress tracking)
   - `i` for ideas
2. **Work**: `SPC g w` to view current work file
3. **Complete**: Mark items DONE with `C-c C-t d`
4. **Archive**: `SPC g A` to move completed items to archive
5. **Review**: `SPC g a` for agenda or `SPC g v` to see archived work

### Blog Workflow
1. Create new post with `SPC B n`
2. Write in org-mode with all its features
3. Publish with `SPC B p`
4. Generated site will be in `~/blog/public/`

## License

This configuration is available under the MIT License.