"use strict";

export const getRecaptchaResponseInternal = useElemId => elemId => () => {
  if (typeof grecaptcha === 'undefined' || !grecaptcha.getResponse) {
    return "";
  }
  if (useElemId) {
    return grecaptcha.getResponse(elemId);
  } else {
    return grecaptcha.getResponse();
  }
}

export const recaptchaRenderInternal = siteKey => elemId => () => {
  // Wait for grecaptcha to be ready before rendering
  if (typeof grecaptcha !== 'undefined' && grecaptcha.ready) {
    grecaptcha.ready(() => {
      const container = document.getElementById(elemId);
      if (container && !container.hasChildNodes()) {
        grecaptcha.render(container, { 'sitekey': siteKey });
      }
    });
  } else {
    // Fallback: poll for grecaptcha to become available
    const checkAndRender = () => {
      if (typeof grecaptcha !== 'undefined' && grecaptcha.ready) {
        grecaptcha.ready(() => {
          const container = document.getElementById(elemId);
          if (container && !container.hasChildNodes()) {
            grecaptcha.render(container, { 'sitekey': siteKey });
          }
        });
      } else {
        setTimeout(checkAndRender, 100);
      }
    };
    checkAndRender();
  }
}
