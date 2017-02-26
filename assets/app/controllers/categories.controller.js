define(['app/app'], function(app) {
    categoriesController.$inject = ['$scope', '$location', 'appState'];
    function categoriesController($scope, $location, appState) {
        $scope.categories= [
            "Vegetable",
            "Fruits",
            "Stationery",
            "Miscellaneous"
        ];
    }

    app.register.controller('categoriesController', categoriesController);
})
