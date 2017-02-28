define(['app/app', 'services', 'directives'], function(app) {
    categoriesController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function categoriesController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {

        getService($scope, '/categories/', {}, 'categories');

        $scope.newCategory = {};

        identityHandlerService().then(function(response){
            $scope.newCategory.user = appState.identity.id;
        });
        alert(JSON.stringify($scope.newCategory));

        $scope.addCategory = function() {
            alert(JSON.stringify($scope.newCategory));
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
