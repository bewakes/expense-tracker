define(['app/app', 'services', 'directives'], function(app) {
    categoriesController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function categoriesController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {
        appState.error = appState.message = null;

        $scope.newCategory = {description:''};

        $scope.reload = function() {
            //getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
            //getService($scope, '/items/', {}, 'items');
            getService($scope, '/categories/', {}, 'categories');
        };

        if(!appState.identity) {
            identityHandlerService().then(function(response){
                $scope.reload();
                $scope.newCategory.organization = appState.current_organization.id;
            });
        }
        else {
            $scope.reload();
            $scope.newCategory.organization = appState.current_organization.id;
        }

        //getService($scope, '/categories/', {}, 'categories');

        $scope.addCategory = function() {
            postService('/categories/', $scope.newCategory)
                .then(function(response) {
                    $scope.reload();
                    $scope.newCategory = {description:'',organization:appState.current_organization.id};
                    appState.message = "Category Added";
                });
        }

        $scope.remove = function(id) {
            deleteService('/categories/'+id, {cagetory:id})
                .then(function(response) {
                    $scope.reload();
                    appState.message = "Category Deleted";
                });
        }
    }

    app.register.controller('categoriesController', categoriesController);
})
