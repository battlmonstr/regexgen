#include "stdafx.h"
#include <string>
#include <unordered_map>
#include <unordered_set>

bool matchString(std::string text) {
	static const std::unordered_map<char, int> transitions[] = {
		std::unordered_map<char, int>({
			{ 'a', 1 },
			{ 'b', 2 },
		}),
		std::unordered_map<char, int>({
			{ 'a', 1 },
			{ 'b', 3 },
		}),
		std::unordered_map<char, int>({
			{ 'a', 1 },
			{ 'b', 2 },
		}),
		std::unordered_map<char, int>({
			{ 'a', 1 },
			{ 'b', 4 },
		}),
		std::unordered_map<char, int>({
			{ 'a', 1 },
			{ 'b', 2 },
		}),
	};
	static const std::unordered_set<int> finalStates = {
		4,
	};
	int currentState = 0;
	for (auto it = text.begin(); it != text.end(); it++) {
	    auto t = transitions[currentState];
	    auto nextState = t.find(*it);
	    if (nextState != t.end()) {
	        currentState = nextState->second;
	    } else {
	        return false;
	    }
	}
	return (finalStates.find(currentState) != finalStates.end());
}
