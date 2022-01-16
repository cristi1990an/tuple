#include <iostream>
#include <string>
#include <vector>

#include "inc/tuple.hpp"

int main()
{
	int a = 5;
	float b = 5.5f;
	std::string c = "5.5f";

	auto t = ctl::make_tuple(a, b, c);

	std::cout << ctl::get<0>(t) << '\n';
	std::cout << ctl::get<1>(t) << '\n';
	std::cout << ctl::get<2>(t) << '\n';

	int d = 5;

	ctl::tuple<int&> ub(d);

	get<0>(ub) = 6;

	std::cout << d << '\n';

	ctl::tuple<size_t, std::string> initializer(5, "Cristi");
	std::vector<std::string> vec = ctl::make_from_tuple<std::vector<std::string>>(initializer);

	for (const auto& word : vec)
	{
		std::cout << word << '\n';
	}

	ctl::tuple<std::string> str = "Cristi";

	std::string temp = ctl::get<std::string>(std::move(str)) + " moved";

	std::cout << ctl::get<std::string>(str) << '\n';
	std::cout << temp << '\n';

	using trivial_mixed_tuple = ctl::tuple<int, double&, const float, char&&, const void*&&>;
	std::cout << typeid(ctl::tuple_element_t<0, trivial_mixed_tuple>).name();
}