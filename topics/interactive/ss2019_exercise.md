# Exercises in JupyterLab 

Here are some simple exercises to try out various things in the JupyterLab interface.

## View this file as rendered markdown.

Right-click on this `ss2019_exercise.md` file and open it as rendered markdown.

## Tabs and splitters

Create a brand new notebook with the "Python 3" kernel. 

Arrange the notebook and rendered markdown side-by-side. Try different window arrangements: one on top of the other; then in a single panel with two tabs. Then split them out again side-by-side.

Open a terminal from the File menu or Launcher. Check which node you are running on with `hostname`.

## Simple notebook operations

- Change the first cell to markdown, and write some text using:
    - Bold
    - Italic
    - [Math](https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference) (try both formatted inlne and formated as formulae): `$f(x) =  a.x^2+b.x+c$`, `$$x_\pm = \frac{-b \pm \sqrt(b^2-4ac)}{2a}$$`
    - Code (triple backtick fences ` ``` `, or indented 4 spaces)
- Create a code cell and evaluate it, printing "Hello Summer School"
    - Experiment with the three run shortcuts `Ctrl-Enter`, `Shift-Enter`, and `Alt-Enter` and note the differences between them.
    - Import `pandas` 
    - Evaluate `pandas?` to get the help on the pandas library.
    - Try `pandas.D<tab>` to get tab completion on the pandas library. Note that completions can take a few second the first time the library get inspected to get result.
    - Complete to `pandas.DataFrame(` place the cursor after the open bracket and press `Shift-Tab` to get quick help.
    - To always see the info about the current function you can open the inspector via the command palette.
        - Use the command palette the find the Keyboard shortcut to open the inspector.
        - Move the inspector tab somewhere so that you can see both it and the notebook.
        - Type `pandas.read_csv(` to see the inspector display function help.
    - Use panda’s `read_csv` to load `'iris.csv'` into a dataframe, display this dataframe
    - open `'iris.csv'` as a standalone CSV file.
    - use `%matplotlib inline` to allow inline graphs, 
    - make a scatter plot of `sepal_length` vs `sepal_width` using `df.plot.[...]`

## Make your scratch folder available in JypyterLab's file browser
It is not possible with the file browser to browse to a directory that is above `$HOME` in the directory structure. However, we might need to access our `$SCRATCH` directory on Daint which resides at `/scratch/snx3000/$USER`.  To do this we can make a sym link to the scratch directory. This can be perfromed at the command line in a terminal, or, could be done with the exclamation mark (bang!) operator directly from a notebook cell. First check that the `$HOME` and `$SCRATCH` variables exist and make a sym link with `!ln -s $SCRATCH $HOME/scratch`. 

## If you have time, or homework

- Move a cell by dragging.
- Try dragging it to another notebook.
- Use the View menu, or click the blue bar, to collapse and expand an input and an output.
- Use the context menu to enable scrolling in a cell that has lots of output.
- Try “Create new view for output” in the context menu of an output. Modify and execute the cell again to see the mirrored output update.
- Learn the various keyboard shortcuts through the menu and the command palette.
- Create a new notebook and try to attach it to the same kernel running in your current notebook.
