// regexmatchertest.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string>
#include <iostream>

using namespace std;

bool matchString(string text);

void matchAndPrint(string text, bool expected) {
	bool matchResult = matchString(text);
	string success = (matchResult == expected) ? "PASS" : "FAIL";
	string showOperator = expected ? "=~" : "!~";
	cout << success << " : " << text << " " << showOperator << " " << "_compiled_pattern_" << endl;
}

int _tmain(int argc, _TCHAR* argv[])
{
	matchAndPrint("abb", true);
	matchAndPrint("babb", true);
	matchAndPrint("aabb", true);
	matchAndPrint("aaaabb", true);
	matchAndPrint("bbbabb", true);
	matchAndPrint("aababababb", true);
	matchAndPrint("babaabaabb", true);
	matchAndPrint("", false);
	matchAndPrint("x", false);
	matchAndPrint("abbc", false);
	matchAndPrint("xabb", false);
	matchAndPrint("ab", false);
	matchAndPrint("aaaab", false);
	matchAndPrint("abababaaab", false);
	return 0;
}

