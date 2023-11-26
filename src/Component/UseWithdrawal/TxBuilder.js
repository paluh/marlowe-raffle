export const uint8ToInt = (uint8) => {
  return ((uint8 << 24) >> 24);
}
