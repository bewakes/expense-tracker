require.config({
  map:{
    // Maps
  },
  paths:{
    // Aliases and paths of modules
    'angular': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min'],
    'jquery': ['https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js'],
    'app': ['app/app'],
    'controllers': ['app/controllers/controllers'],
    'myController': ['app/controllers/myController'],
  },
  shim:{
    // Modules and their dependent modules
    'angular': {
        'exports': 'angular'
    }
  }
});


require(['app', 'services', 'myController', 'controllers'], function(app) {
    app.init();
});
