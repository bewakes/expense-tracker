define(['app/app', 'services', 'directives'], function(app) {
    itemsController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function itemsController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {

        $scope.newItem = {};

        identityHandlerService().then(function(response) {
            $scope.newItem.user = appState.identity.id;
        });

        getService($scope, '/items/', {}, 'items');
        getService($scope, '/categories/', {}, 'categories');


        alert(JSON.stringify($scope.newItem));

        $scope.addItem = function() {
            alert(JSON.stringify($scope.newItem));
            postService('/items/', $scope.newItem)
                .then(function(response) {
                    getService($scope, '/items/', {}, 'items');
                    $scope.newItem = {user:appState.identity.id};
                    appState.message = "Item Added";
                });
        }

        $scope.remove = function(id) {
            deleteService('/items/'+id, {item:id})
                .then(function(response) {
                    getService($scope, '/items/', {}, 'items');
                    appState.message = "Item Deleted";
                });
        }

    }

    app.register.controller('itemsController', itemsController);
})
