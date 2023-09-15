import browserOrNode from 'browser-or-node';
import * as lib from '@dcspark/cardano-multiplatform-lib-browser';

export const importLibImpl = function() {
  return new Promise((resolve, reject) => {
    resolve(lib);
  });
  // if(browserOrNode.isNode) {
  //   console.log("RETURNING NODE LIB");
  //   return import('@dcspark/cardano-multiplatform-lib-nodejs');
  // } else if(browserOrNode.isBrowser) {
  //   console.log("RETURNING BROWSER LIB");
  //   return import('@dcspark/cardano-multiplatform-lib-browser');
  // }
  // return null;
};
