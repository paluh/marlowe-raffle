export const digestImpl = (alg, data) => {
  try {
    return crypto.subtle.digest(alg, data);
  } catch (e) {
    return null;
  }
}

