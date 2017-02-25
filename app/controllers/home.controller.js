define(['app'], function(app) {

    homeController.$inject = ['$scope'];
    function homeController($scope) {
        $scope.name = "bibek";
    }
    app.register.controller('homeController', homeController);
})
