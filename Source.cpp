#include <iostream>
#include <type_traits>
#include <functional>
#include <string>

namespace ctl
{
	template<typename ... Types>
	class tuple;

	template<size_t I, typename ... Types>
	constexpr auto& get(tuple<Types...>& t) noexcept;

	template<size_t I, typename ... Types>
	constexpr const auto& get(const tuple<Types...>& t) noexcept;

	template<size_t I, typename ... Types>
	constexpr auto&& get(tuple<Types...>&& t) noexcept;

	namespace tuple_impl_
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
				constexpr auto& _get_lvalue_ref() noexcept
			{
				return _value;
			}

			template<size_t IndexToGet> requires (IndexToGet == Index)
				constexpr const auto& _get_const_lvalue_ref() const noexcept
			{
				return _value;
			}

			template<size_t IndexToGet> requires (IndexToGet == Index)
				constexpr auto&& _get_rvalue_ref() noexcept
			{
				return std::move(_value);
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
				constexpr auto& _get_lvalue_ref() noexcept
			{
				if constexpr (IndexToGet == Index)
				{
					return _value;
				}
				else
				{
					return _next_inheritence_node::template _get_lvalue_ref<IndexToGet>();
				}
			}

			template<size_t IndexToGet> requires (IndexToGet >= Index)
				constexpr const auto& _get_const_lvalue_ref() const noexcept
			{
				if constexpr (IndexToGet == Index)
				{
					return _value;
				}
				else
				{
					return _next_inheritence_node::template _get_const_lvalue_ref<IndexToGet>();
				}
			}

			template<size_t IndexToGet> requires (IndexToGet >= Index)
				constexpr auto&& _get_rvalue_ref() noexcept
			{
				if constexpr (IndexToGet == Index)
				{
					return std::move(_value);
				}
				else
				{
					return _next_inheritence_node::template _get_rvalue_ref<IndexToGet>();
				}
			}
		};

		template<typename ... Types>
		using tuple_implementation = std::conditional_t<sizeof...(Types) != 1u, tuple_impl_::_indexed_inheritance_node_t<0, Types...>, tuple_impl_::_last_inherited_node_t<0, Types...>>;
	}

	template<typename ... Types>
	class tuple : private tuple_impl_::tuple_implementation<Types...>
	{
	private:
		using tuple_impl_ = std::conditional_t<sizeof...(Types) != 1u, tuple_impl_::_indexed_inheritance_node_t<0, Types...>, tuple_impl_::_last_inherited_node_t<0, Types...>>;
		static constexpr bool nothrow_default_constructible = (std::is_nothrow_default_constructible_v<Types> && ...);

		template<size_t IndexToGet>
		constexpr decltype(auto) _get_lvalue_ref() noexcept
		{
			return tuple_impl_::template _get_lvalue_ref<IndexToGet>();
		}

		template<size_t IndexToGet>
		constexpr decltype(auto) _get_const_lvalue_ref() const noexcept
		{
			return tuple_impl_::template _get_const_lvalue_ref<IndexToGet>();
		}

		template<size_t IndexToGet>
		constexpr decltype(auto) _get_rvalue_ref() noexcept
		{
			return tuple_impl_::template _get_rvalue_ref<IndexToGet>();
		}

		template<size_t ... I, typename ... Args>
		constexpr void _assign_from(std::index_sequence<I...>, const tuple<Args...>& other)
		{
			((_get_lvalue_ref<I>() = other._get_const_lvalue_ref<I>()), ...);
		}

		template<size_t I, typename ... Args>
		friend constexpr auto& get<I, Args...>(tuple<Args...>& t) noexcept;

		template<size_t I, typename ... Args>
		friend constexpr const auto& get<I, Args...>(const tuple<Args...>& t) noexcept;

		template<size_t I, typename ... Args>
		friend constexpr auto&& get<I, Args...>(tuple<Args...>&& t) noexcept;

	public:

		tuple() = default;
		tuple(const tuple&) = default;
		tuple(tuple&&) = default;
		tuple& operator=(const tuple&) = default;
		tuple& operator=(tuple&&) = default;
		~tuple() = default;

		template<typename ... Args> 
			requires (sizeof...(Args) == sizeof...(Types))
			&& (std::constructible_from<Types, Args> && ...)
		constexpr tuple(Args&& ... values)
			: tuple_impl_{ std::forward<Args>(values)... }
		{

		}

		template<typename ... Args> requires (sizeof...(Types) == sizeof...(Args)) && (std::constructible_from<Types, Args> && ...)
			constexpr tuple(const tuple<Args...>& other)
			: tuple_impl_{ std::integral_constant<size_t, 0>{}, other }
		{

		}

		template<typename ... Args> requires (sizeof...(Args) == sizeof...(Types)) && (!(std::same_as<Types, Args> && ...)) && (std::constructible_from<Types, Args> && ...)
			constexpr tuple& operator=(const tuple<Args...>& other)
		{
			_assign_from(std::make_index_sequence<sizeof...(Args)>{}, other);

			return *this;
		}

		static consteval size_t size() noexcept
		{
			return sizeof...(Types);
		}
	};

	template<typename ... Types>
	tuple(Types&&...)->tuple<std::decay_t<Types>...>;

	template<size_t I, typename ... Types>
	constexpr auto& get(tuple<Types...>& t) noexcept
	{
		return t.template _get_lvalue_ref<I>();
	}

	template<size_t I, typename ... Types>
	constexpr const auto& get(const tuple<Types...>& t) noexcept
	{
		return t.template _get_const_lvalue_ref<I>();
	}

	template<size_t I, typename ... Types>
	constexpr auto&& get(tuple<Types...>&& t) noexcept
	{
		return t.template _get_rvalue_ref<I>();
	}

	template<typename ... Args>
	constexpr tuple<Args&...> tie(Args& ... args) noexcept
	{
		return tuple<Args&...>( args... );
	}

	template<typename ... Args>
	constexpr tuple<Args&&...> forward_as_tuple(Args&& ... args) noexcept
	{
		return tuple<Args&&...>(std::forward<Args>(args)...);
	}

	template <typename... Types>
	constexpr tuple<std::unwrap_ref_decay_t<Types>...> make_tuple(Types&&... args)
	{
		return tuple<std::unwrap_ref_decay_t<Types>...>(std::forward<Types>(args)...);
	}

	template<typename T>
	struct is_tuple : std::false_type {};
	
	template<typename... Types>
	struct is_tuple<tuple<Types...>> : std::true_type {};

	template<typename T>
	static constexpr bool is_tuple_v = is_tuple<T>::value;

	template<typename Tuple> requires is_tuple_v<Tuple>
	struct tuple_size : std::integral_constant<size_t, Tuple::size()>
	{

	};

	template<typename Tuple> requires is_tuple_v<Tuple>
	static constexpr size_t tuple_size_v = tuple_size<Tuple>::value;


	namespace tuple_static_asserts_
	{
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

		static_assert(std::is_same_v<decltype(get<2>(std::declval<trivial_tuple&>())),			float&>);
		static_assert(std::is_same_v<decltype(get<2>(std::declval<const trivial_tuple&>())),	const float&>);
		static_assert(std::is_same_v<decltype(get<2>(std::declval<trivial_tuple&&>())),			float&&>);

		static_assert(std::is_same_v<decltype(ctl::tie				(	std::declval<int&>(),	std::declval<float&>(),		std::declval<std::string&>())),																tuple<int&, float&, std::string&>>		);
		static_assert(std::is_same_v<decltype(ctl::forward_as_tuple	(	std::declval<int&&>(),	std::declval<float&&>(),	std::declval<std::string&&>())),															tuple<int&&, float&&, std::string&&>>	);
		static_assert(std::is_same_v<decltype(ctl::make_tuple		(	std::declval<int&>(),	std::declval<float&&>(),	std::declval<const std::string&>(),		std::declval<std::reference_wrapper<double>>())),	tuple<int, float, std::string, double&>>);

		static_assert(is_tuple_v<tuple<int, float, double>>);
		static_assert(is_tuple_v<tuple<int, int>>);
		static_assert(is_tuple_v<tuple<int>>);

		static_assert(tuple_size<tuple<int, float, double>>::value == 3);
		static_assert(tuple_size<tuple<int, int>>::value == 2);
		static_assert(tuple_size<tuple<int>>::value == 1);
	}
}

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
}