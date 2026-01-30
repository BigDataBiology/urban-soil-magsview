// This is called BEFORE your Elm app starts up
// 
// The value returned here will be passed as flags 
// into your `Shared.init` function.
export const flags = () => {

}

// This is called AFTER your Elm app starts up
//
// Here you can work with `app.ports` to send messages
// to your Elm application, or subscribe to incoming
// messages from Elm
export const onReady = ({ app, env }) => {
  let sc = document.createElement('script');
  sc.setAttribute('src', "https://www.googletagmanager.com/gtag/js?id=G-60E4QKLHYR");
  sc.setAttribute('async', true);
  document.getElementById('google-injection-site').appendChild(sc);

  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-60E4QKLHYR');
  gtag('send', 'pageview');
  app.ports.updatePath.subscribe(function(path) {
      gtag('set', 'page', '/'+path);
      gtag('send', 'pageview');
  });


}
