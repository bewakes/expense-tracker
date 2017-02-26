define(['app/app'], function(app) {

    homeController.$inject = ['$scope', '$location', 'appState'];
    function homeController($scope, $location, appState) {
        $scope.name = "bibek";
    }
    app.register.controller('homeController', homeController);
})
