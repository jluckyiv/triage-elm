var config = {
    clientID: "",
    cacheLocation: "localStorage",
    redirectUri: location.origin,
    dev: {
        hearingsUrl: "",
        eventsUrl: "",
        notesUrl: "",
        feedUrl: ""
    },
    prod: {
        hearingsUrl: "",
        eventsUrl: "",
        notesUrl: "",
        feedUrl: ""
    }
};

export default config;