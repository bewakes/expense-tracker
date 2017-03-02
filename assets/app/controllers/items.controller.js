define(['app/app', 'services', 'directives'], function(app) {
    itemsController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function itemsController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {

        appState.error = appState.message = null;

        $scope.reload = function() {
            getService($scope, '/items/', {}, 'items');
            getService($scope, '/categories/', {}, 'categories');
        }

        $scope.newItem = {description:''};

        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                $scope.newItem.organization = appState.current_organization.id;
                $scope.reload();
            });
        }
        else {
            $scope.newItem.organization = appState.current_organization.id;
            $scope.reload();
        }


        $scope.addItem = function() {
            postService('/items/', $scope.newItem)
                .then(function(response) {
                    $scope.reload();
                    $scope.newItem = {description:'',organization:appState.current_organization.id};
                    appState.message = "Item Added";
                });
        }

        $scope.remove = function(id) {
            deleteService('/items/'+id, {item:id})
                .then(function(response) {
                    $scope.reload();
                    appState.message = "Item Deleted";
                });
        }

    }

    app.register.controller('itemsController', itemsController);
})
