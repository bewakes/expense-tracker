define(['app'], function(app) {
    app.controller('myController', myController);

    myController.$inject = ['$scope'];
    function myController($scope) {
        $scope.name = "bibek";
    }
    //return myController;
})
