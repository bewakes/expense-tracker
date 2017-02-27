define(['app/app', 'services'], function(app) {
    categoriesController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'identityHandlerService'];
    function categoriesController($scope, $location, appState, getService, postService, identityHandler) {
        identityHandler(); // gets and handles identity

        getService('/categories', {})
            .then(function(response) {
            });

        $scope.categories= [
            "Vegetable",
            "Fruits",
            "Stationery",
            "Miscellaneous"
        ];
    }

    app.register.controller('categoriesController', categoriesController);
})
