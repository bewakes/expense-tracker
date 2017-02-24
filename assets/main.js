require.config({
  //baseUrl: '',
  map:{
    // Maps
  },
  paths:{
    // Aliases and paths of modules
    'angular': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min'],
    'jquery': ['https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js'],
    'ngRoutes': ['//ajax.googleapis.com/ajax/libs/angularjs/X.Y.Z/angular-route.js'],
    'basecontrollers': ['app/controllers/controllers'],
    'app': ['app/app'],
  },
  shim:{
    // Modules and their dependent modules
    'angular': {
        'exports': 'angular'
    }
  },
  deps:['app']
});


require(['app', 'basecontrollers'], function(app) {
    app.init();
});
