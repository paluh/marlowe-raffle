import { jsx as _jsx } from "react/jsx-runtime";
import { HStack, IconButton, Link, useColorModeValue } from "@chakra-ui/react";
import { FaDiscord, FaTelegramPlane, FaTwitter } from "react-icons/fa/index.js";
const socials = [
    {
        label: "Discord",
        icon: _jsx(FaDiscord, { size: "1.25rem" }),
        href: "https://discord.gg/S7AvwgKVcW",
    },
    {
        label: "Twitter",
        icon: _jsx(FaTwitter, { size: "1.25rem" }),
        href: "https://twitter.com/CNCardano",
    },
    {
        label: "FaTelegramPlane",
        icon: _jsx(FaTelegramPlane, { size: "1.25rem" }),
        href: "https://t.me/+eR_00UAp9PY5N2Iy",
    },
];
export const _SocialMedia = () => {
    const bgColor = useColorModeValue("gray.050", "gray.900");
    return (_jsx(HStack, { alignSelf: "center", children: socials.map((social) => (_jsx(Link, { href: social.href, isExternal: true, children: _jsx(IconButton, { "aria-label": social.label, icon: social.icon, color: "gray.50", background: bgColor, borderRadius: "20px" }) }, social.label))) }));
};
