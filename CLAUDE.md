# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Doom Emacs configuration directory containing personal Emacs settings. Doom Emacs is a configuration framework that provides a curated set of packages and configurations built on top of vanilla Emacs.

## Essential Commands

- `doom sync` - **MUST RUN** after modifying any configuration file (config.el, init.el, or packages.el) to apply changes
- `doom doctor` - Check for common issues in your Doom installation
- `doom upgrade` - Update Doom and its packages
- `doom env` - Regenerate environment variables file

## Configuration Structure

The configuration consists of three main files:

1. **init.el** - Declares which Doom modules to enable/disable
   - Modules are organized by category (completion, ui, editor, etc.)
   - Comment/uncomment lines to enable/disable features
   - Currently uses modern completion (corfu + vertico)
   - Evil mode enabled for Vim keybindings

2. **config.el** - Personal configuration and customizations
   - Place all custom settings here
   - Currently minimal with doom-one theme and line numbers enabled
   - Use `after!` macro for package-specific configurations

3. **packages.el** - Declare additional packages to install
   - Currently empty (using only Doom's built-in packages)
   - Use `package!` to install from MELPA/ELPA
   - Use `unpin!` to get latest versions

## Key Configuration Patterns

- All files use lexical binding (`-*- lexical-binding: t; -*-`)
- Use Doom macros like `after!`, `use-package!`, `add-hook!`
- Configuration changes require `doom sync` to take effect
- Check module documentation with `M-x doom/help-modules`

## Currently Enabled Features

- **Completion**: corfu (in-buffer completion) + vertico (minibuffer completion)
- **Editor**: evil-mode (Vim emulation), file templates, code folding
- **Development**: magit (Git), syntax checking, elisp evaluation
- **Languages**: emacs-lisp, markdown, org-mode, shell scripts
- **UI**: doom theme, dashboard, line numbers, popup management

## Important Notes

- This is a minimal configuration - many powerful Doom modules are available but disabled
- Never edit files in `~/.emacs.d/` directly - only modify files in this directory
- After any configuration change, run `doom sync` in terminal
- For module-specific settings, use `after!` to ensure proper load order