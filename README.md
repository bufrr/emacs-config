# My Doom Emacs Configuration

This is my personal Doom Emacs configuration with a focus on GTD (Getting Things Done) workflow and productivity.

## Features

- 🚀 **GTD Implementation**: Complete Getting Things Done system with interactive inbox processing
- 📝 **Blog System**: Built-in org-mode based blog publishing
- ⚡ **Optimized Keybindings**: Efficient shortcuts for common tasks
- 🎯 **Context-based Organization**: Tasks organized by context (@home, @work, etc.)

## Installation

1. Install [Doom Emacs](https://github.com/doomemacs/doomemacs) first
2. Clone this repository:
   ```bash
   git clone https://github.com/YOUR_USERNAME/doom-config.git ~/.config/doom
   ```
3. Run `doom sync` to install packages and compile
4. Restart Emacs

## GTD Keybindings

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

## Structure

- `config.el` - Main configuration file
- `init.el` - Doom modules declaration
- `packages.el` - Additional package declarations
- `CLAUDE.md` - AI assistant instructions for this codebase

## GTD Files

The GTD system creates these files in `~/org/gtd/`:
- `inbox.org` - Capture destination
- `gtd.org` - Main GTD file (Projects, Next Actions, etc.)
- `calendar.org` - Scheduled appointments

## Customization

Feel free to modify the configuration to suit your needs. Key areas:
- GTD contexts in `org-tag-alist`
- Capture templates in `org-capture-templates`
- Custom agenda views in `org-agenda-custom-commands`

## License

This configuration is available under the MIT License.