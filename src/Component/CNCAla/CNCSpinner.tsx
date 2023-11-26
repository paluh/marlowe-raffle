import { Stack, Spinner, keyframes, Icon } from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { GiForest, GiFruitTree, GiPlantSeed } from "react-icons/gi";
import { FaSeedling } from "react-icons/fa";
import { RiPlantFill } from "react-icons/ri";

export function _CNCSpinner({}) {
	const icons = [GiPlantSeed, RiPlantFill, FaSeedling, GiFruitTree, GiForest];

	const [index, setIndex] = useState(0);
	useEffect(() => {
		setTimeout(() => {
			if (index < 4) {
				setIndex(index + 1);
			} else {
				setIndex(0);
			}
		}, 2000);
	}, [index]);
	const animationKeyframes = keyframes`
    0% { opacity: 0; }
    50% { opacity: 1; }
	70% { opacity: 0.5; }
    100% { opacity: 0; }`;
	const animation = `${animationKeyframes} 2s ease-in-out infinite`;
	return (
		<Stack spacing={-10} align="center">
			{/* <Spinner size="sm" color="brand.200" speed="1s" mb={4}/>*/}
			<Icon
				as={icons[index]}
				w={5}
				h={5}
				color={"green"}
				animation={animation}
				alignSelf="center"
				mb={3.5}
			/>
			<Spinner size="lg" color="orange.400" speed="0.8s" />
			<Spinner size="xl" color="lightgreen" speed="0.6s" />
		</Stack>
	);
}
