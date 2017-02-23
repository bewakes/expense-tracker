define(['angular'], function(angular) {
    var app = angular.module('expenses', []);
    app.init = function() {
        angular.bootstrap(document, 'expenses');
    }
    return app;
});
