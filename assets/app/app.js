define(['angular'], function(angular) {
    var app = angular.module('expenses', ['ngRoute']);
    //app.controller('myController', controller);

    app.config(['$interpolateProvider', '$routeProvider', function($interpolateProvider, $routeProvider){
        //$interpolateProvider.startSymbol('{$');
        //$interpolateProvider.endSymbol('$}');

        $routeProvider
            .when('/', {
                templateUrl: "templates/home.html",
                controller: "myController"
            })
            //.when()
            //.otherwise()
    }]);

    app.init = function() {
        angular.bootstrap(document, ['expenses']);
    }
    return app;
});
