# I broke this shell script when I opened it in notepad
# solution: https://learningintheopen.org/2013/03/07/microsoft-windows-cygwin-error-r-command-not-found/
# To fix:
# Launch Notepad++
# Load the Bash Script File
# Access the menu items “Edit\EOL Conversion\Unix Format”
# Re-save file (File \ Save)

# Download and extract the Windows binary install
# Requires innoextract installed in the Dockerfile
mkdir r-win
wget https://cloud.r-project.org/bin/windows/base/old/3.5.3/R-3.5.3-win.exe \
  --output-document r-win/latest_r.exe
cd r-win
innoextract -e latest_r.exe
mv app/* ../r-win
rm -r app latest_r.exe

# Remove unneccessary files TODO: What else?
rm -r doc tests
