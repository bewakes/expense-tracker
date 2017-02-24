define(['app'], function(app) {
    myController.$inject = ['$scope'];
    function myController($scope) {
        $scope.name = "bibek";
    }
    app.controller('myController', myController);
})
