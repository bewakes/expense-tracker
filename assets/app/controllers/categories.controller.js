define(['app/app', 'services', 'directives'], function(app) {
    categoriesController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function categoriesController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {
        appState.error = appState.message = null;

        $scope.newCategory = {};

        if(!appState.identity) {
            identityHandlerService().then(function(response){
                $scope.newCategory.organization = appState.current_organization.id;
            });
        }
        else {
            $scope.newCategory.organization = appState.current_organization.id;
        }

        getService($scope, '/categories/', {}, 'categories');

        $scope.addCategory = function() {
            postService('/categories/', $scope.newCategory)
                .then(function(response) {
                    getService($scope, '/categories/', {}, 'categories');
                    $scope.newCategory = {user:appState.identity.id};
                    appState.message = "Category Added";
                });
        }

        $scope.remove = function(id) {
            deleteService('/categories/'+id, {cagetory:id})
                .then(function(response) {
                    getService($scope, '/categories/', {}, 'categories');
                    appState.message = "Category Deleted";
                });
        }
    }

    app.register.controller('categoriesController', categoriesController);
})
