define(['app/app', 'services', 'directives'], function(app) {
    settingsController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];

    function settingsController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        $scope.searchUserList = ["bibek", "pandey"];
    }

    app.register.controller('settingsController', settingsController);

});
