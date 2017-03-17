define(['angular', 'ngRoute', 'ngCookies', 'angular-nvd3'], function(angular) {
    var app = angular.module('expenses', ['ngRoute', 'ngCookies', 'nvd3']);

    var appState = {
        error: '',
        identity: null,
        username: 'bibek',
    };

    app.value('appState', appState);

    config.$inject = ['$routeProvider', '$controllerProvider', '$provide', '$compileProvider', '$httpProvider', '$locationProvider'];
    app.config(config);

    function config($routeProvider, $controllerProvider, $provide, $compileProvider, $httpProvider, $locationProvider) {
        $httpProvider.defaults.xsrfCookieName = 'csrftoken';
        $httpProvider.defaults.xsrfHeaderName = 'X-CSRFToken';
        app.register = {
            controller: $controllerProvider.register,
            factory: $provide.factory,
            service: $provide.service,
            directive: $compileProvider.directive
        };

        $routeProvider
            .when('/', resolve('home', 'home'))
            .when('/_=_', resolve('home', 'home')) // for social login redirect
            .when('/items', resolve('items', 'items'))
            .when('/categories', resolve('categories', 'categories'))
            .when('/expenses', resolve('expenses', 'expenses'))
            .when('/settings', resolve('settings', 'settings'))
            .when('/feedback', resolve('feedback', 'feedback'))
    }

    function resolve(controllername, templatename) {
        return {
            templateUrl: '/static/app/templates/'+templatename+'.html',
            controller: controllername+'Controller',
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
