define(['angular', 'ngRoute', 'ngCookies'], function(angular) {
    var app = angular.module('expenses', ['ngRoute', 'ngCookies']);

    var appState = {
        error: '',
        identity: null,
        username: 'bibek',
    };

    app.value('appState', appState);

    config.$inject = ['$routeProvider', '$controllerProvider', '$provide', '$compileProvider', '$httpProvider', '$locationProvider'];
    app.config(config);

    function config($routeProvider, $controllerProvider, $provide, $compileProvider, $locationProvider) {

        app.register = {
            controller: $controllerProvider.register,
            factory: $provide.factory,
            directive: $compileProvider.directive
        };

        $routeProvider
            .when('/', resolve('home', 'home'))
            .when('/_=_', resolve('home', 'home')) // for social login redirect
            .when('/items', resolve('items/listItems', 'items/listItems'))
            .when('/items/new', resolve('items/newItem', 'items/newItem'))
    }

    function resolve(controllername, templatename) {
        var arr = controllername.split('/');
        var controller = arr[arr.length-1];
        return {
            templateUrl: 'static/app/templates/'+templatename+'.html',
            controller: controller+'Controller',
            resolve: {
                load: function($q, $rootScope) {
                    var deferred = $q.defer();
                    require(['app/controllers/'+controllername+'.controller'], function() {
                        $rootScope.$apply(deferred.resolve);
                    });
                    return deferred.promise;
                }
            }
        }
    }

    return app;
});
