# https://rnbeads.org/data/installing_rnbeads.html

# Download ghostscript from http://ghostscript.com/download/gsdnld.html
# After installing ghostscript, we still need to tell R where to find ghostscript. To do so, it is necessary to set adapt your system’s Path variable:
# Go to Control Panel → System and Security → System → Advanced System Settings → computer name, domain and workgroup settings → Advanced → Environment Variables
# Find the Path variable within System Variables, select it and click on edit.
# Add C:\Program Files\gs\gs9.23\bin (or the directory where you installed ghostscript to) to the Path variable. In Windows 10, you can do this by clicking on New and entering the path. In other Windows versions, just append the path to the variable value, seperated by a semicolon.
# Restart R

shiny::runGist("https://gist.github.com/haozhu233/9e675e1a8a1bb4744f9ebc9246a2366b")


# https://stackoverflow.com/questions/71403688/save-kable-in-r-does-not-compile-latex-table/71542939#71542939