import { Button, ChakraBaseProvider, Center, Container, Heading, Stack, Text, Modal, ModalOverlay, ModalContent, ModalHeader, ModalCloseButton, ModalBody, ModalFooter, HStack, Link, Select } from "@chakra-ui/react";
import { extendBaseTheme, useColorModeValue, useBreakpointValue } from "@chakra-ui/react";
import { theme as proTheme } from "@chakra-ui/pro-theme";

export const _Button = Button;

export const _Center = Center;

export const _ChakraProvider = ChakraBaseProvider;

export const _Container = Container;

export const _Stack = Stack;

export const _Heading = Heading;

export const _Text = Text;

// export all the extra components from Chakra UI

export const _Modal = Modal;

export const _ModalOverlay = ModalOverlay;

export const _ModalContent = ModalContent;

export const _ModalHeader = ModalHeader;

export const _ModalCloseButton = ModalCloseButton;

export const _ModalBody = ModalBody;

export const _ModalFooter = ModalFooter;

export const _HStack = HStack;

export const _Link = Link;

export const _Select = Select;

import "@fontsource/libre-franklin";

const overrides = {
  config: {
      initialColorMode: "dark",
      useSystemColorMode: false,
  },
  colors: {
    ...proTheme.colors,
    brand: {
      highlight: "#4CEEFD",
      light: "#7928CA",
      dark: "#FF0080",
      "50": "#50D9FA",
      "100": "#54BCE4",
      "200": "#0A87E3",
      "300": "#272F91",
      "400": "#2C59A5",
      "500": "#292EC0",
      "600": "#182AA7",
      "700": "#13154E",
      "800": "#0B0F53",
      "900": "#000000",
    },
  },
  fonts: {
    heading: "Libre Franklin, -apple-system, system-ui, sans-serif",
    body: "Libre Franklin, -apple-system, system-ui, sans-serif",
  }
};

export const theme = extendBaseTheme(proTheme, overrides);

export const useBreakpointValue_ = useBreakpointValue;

export const useColorModeValue_ = useColorModeValue;
