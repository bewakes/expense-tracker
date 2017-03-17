require.config({
  map:{
    // Maps
  },
  paths:{
    // Aliases and paths of modules
    'angular': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min', 'lib/angular.min'],
    'jquery': ['https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min'],
    //'jquery-ui': ['//code.jquery.com/ui/1.12.1/jquery-ui'],
    'ngRoute': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-route'],
    'ngCookies': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.0/angular-cookies'],
    'angular-nvd3': ['https://cdnjs.cloudflare.com/ajax/libs/angular-nvd3/1.0.9/angular-nvd3.min', 'lib/angular-nvd3.min'],
    'd3': ['lib/d3.v3.min'],
    'nvd3': ['lib/nv.d3.min'],
    'services': ['app/services'],
    'directives': ['app/directives'],
  },
  shim:{
    // Modules and their dependent modules
    'angular': {
        'exports': 'angular'
    },
    'ngRoute': {
        'deps': ['angular'],
        'exports': 'ngRoute'
    },
    'ngCookies': {
        'deps': ['angular'],
        'exports': 'ngCookies'
    },
    'd3': {
        'exports': 'd3'
    },
    'nvd3': {
        'deps': ['d3'],
        'exports': 'nvd3'
    },
    'angular-nvd3': {
        'exports':'angular-nvd3',
        'deps':['nvd3', 'angular']
    },
	'jquery': {
        'exports': 'jquery'
    },
    /*'jquery-ui': {*/
        //'deps': ['jquery'],
        //'exports': 'jquery-ui'
    /*}*/
  },
  //deps:['main']
});

// this was in main.js, which is now removed
define(['angular', 'app/app'], function(angular) {
    angular.bootstrap(document.body, ['expenses']);
});
