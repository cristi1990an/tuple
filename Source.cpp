#include <iostream>
#include <type_traits>
#include <string>

template<typename ... Types>
class tuple;

namespace _tuple_impl
{
	template<size_t Index, typename MyType, typename ... Unused>
	class _last_inherited_node_t
	{
	private:
		MyType _value;

	protected:
		_last_inherited_node_t() = default;
		_last_inherited_node_t(const _last_inherited_node_t&) = default;
		_last_inherited_node_t(_last_inherited_node_t&&) = default;
		_last_inherited_node_t& operator=(const _last_inherited_node_t&) = default;
		_last_inherited_node_t& operator=(_last_inherited_node_t&&) = default;
		~_last_inherited_node_t() = default;

		template<typename MyTypeArg> requires std::constructible_from<MyType, MyTypeArg>
		constexpr _last_inherited_node_t(MyTypeArg&& my_value)
			: _value{ std::forward<MyTypeArg>(my_value) }
		{

		}

		template<size_t Index, typename ... Args>
		constexpr _last_inherited_node_t(std::integral_constant<size_t, Index>, const tuple<Args...>& tuple)
			: _value{ static_cast<MyType>(tuple.get<Index>()) }
		{

		}

		template<size_t IndexToGet> requires (IndexToGet == Index)
		constexpr MyType& get() noexcept
		{
			return _value;
		}

		template<size_t IndexToGet> requires (IndexToGet == Index)
		constexpr const MyType& get() const noexcept
		{
			return _value;
		}
	};

	template<size_t Index, typename MyType, typename ... Others> 
	class _indexed_inheritance_node_t : protected std::conditional_t<
		sizeof...(Others) != 1,
		_indexed_inheritance_node_t<Index + 1, Others...>, 
		_last_inherited_node_t<Index + 1, Others...>>
	{
	private:
		MyType _value;

		using _next_inheritence_node = std::conditional_t<sizeof...(Others) != 1, _indexed_inheritance_node_t<Index + 1, Others...>, _last_inherited_node_t<Index + 1, Others...>>;
		
	protected:
		
		_indexed_inheritance_node_t() = default;
		_indexed_inheritance_node_t(const _indexed_inheritance_node_t&) = default;
		_indexed_inheritance_node_t(_indexed_inheritance_node_t&&) = default;
		_indexed_inheritance_node_t& operator=(const _indexed_inheritance_node_t&) = default;
		_indexed_inheritance_node_t& operator=(_indexed_inheritance_node_t&&) = default;
		~_indexed_inheritance_node_t() = default;

		template<typename MyTypeArg, typename ... Args>  requires std::constructible_from<MyType, MyTypeArg>
		constexpr _indexed_inheritance_node_t(MyTypeArg&& my_value, Args&& ... others)
			: _next_inheritence_node{ std::forward<Args>(others)... }
			, _value{ std::forward<MyTypeArg>(my_value) }
		{

		}

		template<size_t Index, typename ... Args>
		constexpr _indexed_inheritance_node_t(std::integral_constant<size_t, Index>, const tuple<Args...>& other)
			: _next_inheritence_node{ std::integral_constant<size_t, Index + 1>{}, other }
			, _value{ static_cast<MyType>(other.get<Index>()) }
		{

		}

		template<size_t IndexToGet> requires (IndexToGet >= Index)
		constexpr auto& get() noexcept
		{
			if constexpr (IndexToGet == Index)
			{
				return _value;
			}
			else
			{
				return _next_inheritence_node::template get<IndexToGet>();
			}
		}

		template<size_t IndexToGet> requires (IndexToGet >= Index)
		constexpr const auto& get() const noexcept
		{
			if constexpr (IndexToGet == Index)
			{
				return _value;
			}
			else
			{
				return _next_inheritence_node::template get<IndexToGet>();
			}
		}
	};
}

template<typename ... Types> 
class tuple : private std::conditional_t<
	sizeof...(Types) != 1u,
	_tuple_impl::_indexed_inheritance_node_t<0, Types...>,
	_tuple_impl::_last_inherited_node_t<0, Types...>>
{
	using _tuple_impl = std::conditional_t<sizeof...(Types) != 1u, _tuple_impl::_indexed_inheritance_node_t<0, Types...>, _tuple_impl::_last_inherited_node_t<0, Types...>>;

	static constexpr bool nothrow_default_constructible = (std::is_nothrow_default_constructible_v<Types> && ...);

public:
	template<size_t IndexToGet>
	constexpr auto& get()
	{
		return _tuple_impl::template get<IndexToGet>();
	}

	template<size_t IndexToGet>
	constexpr const auto& get() const
	{
		return _tuple_impl::template get<IndexToGet>();
	}

private:

	template<size_t ... I, typename ... Args>
	constexpr void _assign_from(std::index_sequence<I...>, const tuple<Args...>& other)
	{
		((get<I>() = other.get<I>()), ...);
	}

public:

	tuple() = default;
	tuple(const tuple&) = default;
	tuple(tuple&&) = default;
	tuple& operator=(const tuple&) = default;
	tuple& operator=(tuple&&) = default;
	~tuple() = default;

	template<typename ... Args> requires (sizeof...(Args) == sizeof...(Types))
	constexpr tuple(Args&& ... values)
		: _tuple_impl { std::forward<Args>(values)... }
	{

	}

	template<typename ... Args> requires (sizeof...(Types) == sizeof...(Args)) && (std::constructible_from<Types, Args> && ...)
	constexpr tuple(const tuple<Args...>& other)
		: _tuple_impl{ std::integral_constant<size_t, 0>{}, other }
	{

	}

	template<typename ... Args> requires (sizeof...(Args) == sizeof...(Types)) && (std::constructible_from<Types, Args> && ...)
	constexpr tuple& operator=(const tuple<Args...>& other)
	{
		_assign_from(std::make_index_sequence<sizeof...(Args)>{}, other);

		return *this;
	}
};

template<typename ... Types>
tuple(Types&&...)->tuple<std::decay_t<std::remove_reference_t<Types>>...>;

using trivial_tuple = tuple<int, double, float, char, void*>;

static_assert(std::is_trivially_constructible_v<trivial_tuple>);
static_assert(std::is_trivially_default_constructible_v<trivial_tuple>);
static_assert(std::is_trivially_copy_constructible_v<trivial_tuple>);
static_assert(std::is_trivially_move_constructible_v<trivial_tuple>);
static_assert(std::is_trivially_copyable_v<trivial_tuple>);
static_assert(std::is_trivially_copy_assignable_v<trivial_tuple>);
static_assert(std::is_trivially_move_assignable_v<trivial_tuple>);
static_assert(std::is_trivially_destructible_v<trivial_tuple>);
static_assert(std::is_trivial_v<trivial_tuple>);

tuple<int, float, const char*> get_tuple()
{
	return { 1, 2.2f, "Cristi" };
}

struct Test
{
	Test()
	{
		std::cout << "Default constructor\n";
	}
	Test(const char*)
	{
		std::cout << "Const char* constructor\n";
	}
	Test(const Test&)
	{
		std::cout << "Copy constructor\n";
	}
	Test(Test&&) noexcept
	{
		std::cout << "Move constructor\n";
	}
	Test& operator=(const Test&)
	{
		std::cout << "Copy assignmet operator\n";
		return *this;
	}
	Test& operator=(Test&&) noexcept
	{
		std::cout << "Move assignment operator\n";
		return *this;
	}
	~Test()
	{
		std::cout << "Destructor\n";
	}
	std::string message() const
	{
		return "Cristi";
	}
};

int main()
{
	tuple<Test, Test> source;

	auto result = std::move(source);



}