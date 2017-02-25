require.config({
  map:{
    // Maps
  },
  paths:{
    // Aliases and paths of modules
    'angular': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min', 'lib/angular.min'],
    'jquery': ['https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js'],
    'ngRoute': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-route'],
    'ngCookies': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.0/angular-cookies'],
  },
  shim:{
    // Modules and their dependent modules
    'angular': {
        'exports': 'angular'
    }
  },
  //deps:['main']
});

// this was in main.js, which is now removed
define(['angular', 'app'], function(angular) {
    angular.bootstrap(document.body, ['expenses']);
});
