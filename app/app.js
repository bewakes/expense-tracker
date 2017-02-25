define(['angular', 'ngRoute'], function(angular) {
    var app = angular.module('expenses', ['ngRoute']);

    config.$inject = ['$routeProvider', '$controllerProvider', '$provide', '$compileProvider', '$httpProvider'];
    app.config(config);

    function config($routeProvider, $controllerProvider, $provide, $compileProvider) {

        app.register = {
            controller: $controllerProvider.register,
            factory: $provide.factory,
            directive: $compileProvider.directive
        };

        $routeProvider
            .when('/', resolve('home', 'home'))
            .when('/login/', resolve('login', 'login'))
    }

    function resolve(controllername, templatename) {
        return {
            templateUrl: 'static/templates/'+templatename+'.html',
            controller: controllername+'Controller',
            resolve: {
                load: function($q, $rootScope) {
                    var deferred = $q.defer();
                    require(['controllers/'+controllername+'.controller'], function() {
                        $rootScope.$apply(deferred.resolve);
                    });
                    return deferred.promise;
                }
            }
        }
    }

    return app;
});
