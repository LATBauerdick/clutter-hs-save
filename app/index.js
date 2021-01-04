const { app, BrowserWindow, dialog, messageNode, systemPreferences, MenuItem, shell } = require('deskgap');
const fs = require('fs');

app.once('ready', () => {

    // var fns = ["161314.html", "5253301.html", "19152349.html", "77555126.html", "117270609.html"];
    var fns = ["album/5199109", "album/4650811", "album/3747973", "album/8384784", "album/14238068"];
    var uh = "http://localhost:8080/"
    var win = Array(5);
    var i;
    for (j=0; j < 3; j++) {
      nx = 250+j*80;
      ny=100+j*20;
      for (i = 0; i < fns.length; i++) {
        win[i] = new BrowserWindow({
            menu: null,
            show: false,
            width: 150,
            height: 150,
            titleBarStyle: 'hidden',
            resizable: false,
            minimizable: false
        }).once('ready-to-show', function() {
            this.show();
        });
        win[i].setPosition(nx+20*i,ny+20*i);
        win[i].loadURL(uh + fns[i]);
    };
    };

    const win0 = new BrowserWindow();
    win0.setTitleBarStyle('hidden');
    // win0.setSize(150,150);
    win0.setPosition(100,100);
    win0.loadURL('http://localhost:8080/albums');

    systemPreferences.on('dark-mode-toggled', () => {
        win.webContents.send('dark-mode-toggled', systemPreferences.isDarkMode());
    });


});

