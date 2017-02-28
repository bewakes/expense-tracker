require.config({
  map:{
    // Maps
  },
  paths:{
    // Aliases and paths of modules
    'angular': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min', 'lib/angular.min'],
    'jquery': ['https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min'],
    'jquery-ui': ['//code.jquery.com/ui/1.12.1/jquery-ui'],
    'ngRoute': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.2.1/angular-route'],
    'ngCookies': ['https://ajax.googleapis.com/ajax/libs/angularjs/1.5.0/angular-cookies'],
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
	'jquery': {
        'exports': 'jquery'
    },
    'jquery-ui': {
        'deps': ['jquery'],
        'exports': 'jquery-ui'
    }
  },
  //deps:['main']
});

// this was in main.js, which is now removed
define(['angular', 'app/app', 'jquery', 'jquery-ui'], function(angular) {
    angular.bootstrap(document.body, ['expenses']);

    //var $j = jQuery.noConflict();

    $('input[type=date]').datepicker();

});
