import 'font-awesome/css/font-awesome.css';
import 'bootstrap/dist/css/bootstrap.css';

import './main.css';
import config from './config';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

// Graph API endpoint to show user profile
var graphApiEndpoint = "https://graph.microsoft.com/v1.0/me";

// Graph API scope used to obtain the access token to read user profile
var graphAPIScopes = ["https://graph.microsoft.com/user.read"];

// Initialize application
var userAgentApplication = new Msal.UserAgentApplication(config.msal.clientID, null, loginCallback, {
    redirectUri: config.msal.redirectUri,
    cacheLocation: config.msal.cacheLocation
});


window.onload = function () {
    // If page is refreshed, continue to display user info
    if (!userAgentApplication.isCallback(window.location.hash) && window.parent === window && !window.opener) {
        getUser();
    }
}

function getUser() {
    var user = userAgentApplication.getUser();
    if (user) {
        getUserInfo(user);
    }
}

/**
 * Call the Microsoft Graph API and store the user
 */
function getUserInfo(user) {
    if (!user) {
        userAgentApplication.loginRedirect(graphAPIScopes);
    } else {
        userAgentApplication.acquireTokenSilent(graphAPIScopes)
            .then(function (token) {
                callWebApiWithTokenAndCallback(graphApiEndpoint, token);

            }, function (error) {
                // If the acquireTokenSilent() method fails, then acquire the token interactively via acquireTokenRedirect().
                // In this case, the browser will redirect user back to the Azure Active Directory v2 Endpoint so the user 
                // can re-type the current username and password and/ or give consent to new permissions your application is requesting.
                // After authentication/ authorization completes, this page will be reloaded again and callGraphApi() will be called.
                // Then, acquireTokenSilent will then acquire the token silently, the Graph API call results will be made and results will be displayed in the page.
                if (error) {
                    userAgentApplication.acquireTokenRedirect(graphAPIScopes);
                    // userAgentApplication.acquireTokenPopup(graphAPIScopes);
                }
            });

    }
}

/**
 * Show an error message in the page
 * @param {string} endpoint - the endpoint used for the error message
 * @param {string} error - the error string
 * @param {object} errorElement - the HTML element in the page to display the error
 */
function logError(endpoint, error, errorDesc) {
    var formattedError = JSON.stringify(error, null, 4);
    if (formattedError.length < 3) {
        formattedError = error;
    }
    console.error(error);
}

/**
 * Callback method from sign-in: if no errors, call callGraphApi() to show results.
 * @param {string} errorDesc - If error occur, the error message
 * @param {object} token - The token received from login
 * @param {object} error - The error 
 * @param {string} tokenType - The token type: For loginRedirect, tokenType = "id_token". For acquireTokenRedirect, tokenType:"access_token"
 */
function loginCallback(errorDesc, token, error, tokenType) {
    if (errorDesc) {
        logError(msal.authority, error, errorDesc);
    } else {
        console.log("longCallback calls getUserInfo()")
        getUserInfo();
    }
}

/**
 * Call a Web API using an access token.
 * 
 * @param {any} endpoint - Web API endpoint
 * @param {any} token - Access token
 */
function callWebApiWithTokenAndCallback(endpoint, token) {
    var headers = new Headers();
    var bearer = "Bearer " + token;
    headers.append("Authorization", bearer);
    var options = {
        method: "GET",
        headers: headers
    };

    fetch(endpoint, options)
        .then(function (response) {
            var contentType = response.headers.get("content-type");
            if (response.status === 200 && contentType && contentType.indexOf("application/json") !== -1) {
                response.json()
                    .then(function (data) {
                        saveUser(data);
                    })
                    .catch(function (error) {
                        logError(endpoint, error);
                    });
            } else {
                response.json()
                    .then(function (data) {
                        logError(endpoint, data);
                    })
                    .catch(function (error) {
                        logError(endpoint, error);
                    });
            }
        })
        .catch(function (error) {
            logError(endpoint, error);
        });
}


var flags = { host: window.location.host, user: window.localStorage.getItem("user") || null };
var app = Main.embed(document.getElementById("root"), flags);

app.ports.login.subscribe(function (value) {
    app.ports.loginResult.send(getUserInfo());
});


app.ports.logout.subscribe(function (value) {
    userAgentApplication.logout();
    localStorage.setItem("user", value);
});


function saveUser(data) {
    localStorage.setItem("user", JSON.stringify(data));
    app.ports.loginResult.send(data);
}


registerServiceWorker();
