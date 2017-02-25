define(['app'], function(app) {
    itemsController.$inject = ['$scope', '$location', 'appState'];
    function itemsController($scope, $location, appState) {
        $scope.type = "Items";
    }

    app.register.controller('itemsController', itemsController);
})
