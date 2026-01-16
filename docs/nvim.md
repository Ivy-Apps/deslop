# 🚀 NeoVim Quickstart & Cheatsheet

## 1. The Basics (Read This First)

### How to Launch
Since we use Nix to manage our dependencies, you must enter the environment first to get the correct compiler and tools.

1. Open your terminal.
2. Navigate to the project folder.
3. Run: `nix develop` (This loads GHC, HLS, and NeoVim).
4. Run: `vim .` (This opens NeoVim in the current directory).

### The "Leader" Key
You will see references to `<Space>` in this guide.
* **What is it?** It is your custom trigger key (The "Leader" key).
* **Which key is it?** We have mapped it to **Space Bar**.

### 🛑 STOP: Don't Hold Keys!
**"Should I press Shift + Space to execute commands?"**
**NO.**

Unlike IntelliJ where you hold `Cmd+Shift+P` (Chords), NeoVim relies on **Sequences**.
* **Do NOT hold Space.**
* **Do this instead:** Tap `Space`, release it, then tap `f`, then tap `f`.
    * Think of it like a combo in a fighting game (e.g., *Street Fighter*), not a shortcut in Word.

---

## 2. For IntelliJ Users: The Translation Layer
You are used to powerful, magic shortcuts. NeoVim has them too, but they are composed differently.

| IntelliJ Action | NeoVim Equivalent | Notes |
| :--- | :--- | :--- |
| **Search Everywhere** (Double Shift) | `<Space>ff` | Finds files by name. |
| **Find in Files** (Cmd+Shift+F) | `<Space>fg` | "Find Grep" - searches text content. |
| **Show Intentions** (Opt+Enter) | `<Space>ca` | "Code Action" (Import, Fill Hole, etc). |
| **Reformat Code** (Cmd+Opt+L) | `<Space>fm` | Runs Fourmolu. |
| **Go to Definition** (Cmd+Click) | `gd` | "Go Definition". |
| **Back** (Cmd+[) | `Ctrl` + `o` | Jumps back to where you were. |
| **Project View** (Cmd+1) | `<Space>e` | Toggles the file tree. |
| **Terminal** (Opt+F12) | `:terminal` | Opens integrated shell. |

---

## 3. Auto-Complete & Snippets
We know you love `TAB`. We have configured NeoVim to support this, but the mechanics are slightly different from IntelliJ.

### How it works
1.  **Start Typing:** The completion menu pops up automatically.
2.  **Navigate:** Press **`TAB`** to move *down* the list (or `Shift+TAB` to move up).
3.  **Confirm:** Press **`ENTER`** to select the item.

> **⚠️ Crucial Difference:**
> In IntelliJ, `TAB` often selects *and* inserts.
> In our NeoVim setup, **`TAB` only cycles** through options. You must hit **`ENTER`** to insert the code.

| Key | Action |
| :--- | :--- |
| `TAB` | Select next item in list |
| `Shift`+`TAB` | Select previous item |
| `Enter` | **Confirm / Insert selection** |
| `Ctrl`+`Space` | Manually trigger completion menu |

---

## 4. The Philosophy: "Verbs and Nouns"
In NeoVim, you don't just type; you command. Commands are often structured as **Action + Motion**.

* **The Action (Verb):** `d` (delete), `c` (change), `y` (yank/copy), `v` (visual select).
* **The Motion (Noun):** `w` (word), `$` (end of line), `i(` (inside parenthesis).

**Examples:**
* `dw` $\rightarrow$ **D**elete **W**ord.
* `ci"` $\rightarrow$ **C**hange **I**nside **"**quotes**"** (deletes content inside quotes and puts you in Insert mode).
* `y$` $\rightarrow$ **Y**ank (Copy) to **$** (end of line).

---

## 5. The Cheatsheet

| Category | Key Sequence | Action |
| :--- | :--- | :--- |
| **Files & Search** | `<Space>ff` | **F**ind **F**iles (Fuzzy search file names) |
| | `<Space>fg` | **F**ind **G**rep (Search text inside all files) |
| | `<Space>e` | Toggle File Tree Explorer |
| | `:e <path>` | Open a specific file path |
| **Navigation** | `h` `j` `k` `l` | Left, Down, Up, Right (Use these, not arrows!) |
| | `w` / `b` | Jump forward/backward by **w**ord |
| | `}` / `{` | Jump down/up by paragraph (useful for blocks) |
| | `gd` | **G**o to **D**efinition (LSP) |
| | `Ctrl` + `o` | Go back to previous jump location |
| **Editing** | `i` | **I**nsert mode (start typing at cursor) |
| | `a` | **A**ppend mode (start typing *after* cursor) |
| | `o` | **O**pen new line below and insert |
| | `u` | **U**ndo |
| | `Ctrl` + `r` | **R**edo |
| **Manipulating** | `yy` | **Y**ank (Copy) current line |
| | `dd` | **D**elete (Cut) current line |
| | `p` | **P**aste after cursor |
| | `cc` | **C**hange line (clears line and enters Insert mode) |
| | `gcc` | Toggle **C**omment on current line |
| **Haskell Specific** | `K` | Hover documentation (types/docs) |
| | `<Space>fm` | **F**or**m**at file (Fourmolu) |
| | `<Space>ch` | Search Hoogle for signature |
| | `<Space>ca` | **C**ode **A**ction (Fix imports, etc.) |
| **Window/System** | `:w` | Save (Write) |
| | `:q` | Quit |
| | `Ctrl` + `w`, then `v` | Split window **V**ertically |
| | `Ctrl` + `w`, then `h/j/k/l`| Move focus between split windows |

---

## 6. The Three Main Modes
If you try to type code immediately after opening NeoVim, nothing will happen (or you'll accidentally delete things). You need to know which **Mode** you are in. Look at the bottom left of the screen.

### 🎮 Normal Mode (The "Controller")
* **Status Bar:** (Empty) or `NORMAL`
* **Purpose:** Moving around, deleting lines, changing files. **You cannot type text here.**
* **How to get here:** Press `Esc`. (If in doubt, mash `Esc` three times).
* **Default State:** NeoVim always starts in this mode.

### 📝 Insert Mode (The "Notepad")
* **Status Bar:** `-- INSERT --`
* **Purpose:** Typing code just like in IntelliJ.
* **How to get here:** Press `i` (insert at cursor) or `a` (append after cursor) while in Normal Mode.
* **How to leave:** Press `Esc` to go back to Normal Mode.

### ⚙️ Command Mode (The "Menu")
* **Status Bar:** `:` (At the very bottom left)
* **Purpose:** Saving files, quitting, or running searching/replace commands.
* **How to get here:** Press `:` while in Normal Mode.
* **Example:** Type `:w` and hit `Enter` to save.

---

## 7. MacOS Optimization (Optional)

Since you are on MacOS, the default keyboard "Repeat Rate" is often too slow for Vim navigation (holding `j` to scroll down will feel sluggish).

1. Open **System Settings** -> **Keyboard**.
2. Set **Key repeat rate** to **Fast** (Max).
3. Set **Delay until repeat** to **Short** (Max).

*This makes moving around with `h` `j` `k` `l` feel instantaneous.*