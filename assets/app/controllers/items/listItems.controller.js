define(['app/app'], function(app) {
    listItemsController.$inject = ['$scope', '$location', 'appState'];
    function listItemsController($scope, $location, appState) {
        $scope.category_items = {
            "Vegetable": ["Brinjal", "Tomato", "Potato", "Bean", "Spinach"],
            "Fruits": ["Banana", "Apple", "Orange", "Grapes", "Papaya"],
            "Stationery": ["Pen", "Pencil", "A4 Paper", "Sketch Book"],
            "Miscellaneous": ["Nail Cutter", "Heater", "Violin"]
        }

        $scope.category_rows = [];
        $scope.categories = Object.keys($scope.category_items).map(function(e) { return {name:e};});

        $scope.top_items = [1,2,3,4,5];
        $scope.latest_items = [1,2,3,4];

    }

    app.register.controller('listItemsController', listItemsController);
})
