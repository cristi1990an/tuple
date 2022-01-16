#pragma once

#include <type_traits>
#include "tuple_implementation.hpp"

namespace ctl
{
	template<size_t I, typename ... Types>
	constexpr auto& get(tuple<Types...>& t) noexcept
	{
		return t.template _get_lvalue_ref<I>();
	}

	template<typename Type, typename ... Types>
	constexpr auto& get(tuple<Types...>& t) noexcept
	{
		return t.template _get_lvalue_ref<Type>();
	}

	template<size_t I, typename ... Types>
	constexpr const auto& get(const tuple<Types...>& t) noexcept
	{
		return t.template _get_const_lvalue_ref<I>();
	}

	template<typename Type, typename ... Types>
	constexpr const auto& get(const tuple<Types...>& t) noexcept
	{
		return t.template _get_const_lvalue_ref<Type>();
	}

	template<size_t I, typename ... Types>
	constexpr auto&& get(tuple<Types...>&& t) noexcept
	{
		return t.template _get_rvalue_ref<I>();
	}

	template<typename Type, typename ... Types>
	constexpr auto&& get(tuple<Types...>&& t) noexcept
	{
		return t.template _get_rvalue_ref<Type>();
	}

	template<typename Tuple> requires is_tuple<Tuple>
	struct tuple_size;

	template<size_t Index, typename T>
	struct tuple_element;

	template<typename T, typename Tuple>
	concept constructible_from_tuple_types = []<size_t ... I>(std::index_sequence<I...>) -> bool
	{
		return std::is_constructible_v<T, decltype(get<I>(std::declval<Tuple>()))...>;
	}(std::make_index_sequence <tuple_size_v<std::decay_t<Tuple>>>());

	template<typename ... Types>
	class tuple : private tuple_impl_::tuple_implementation<Types...>
	{
	private:
		using tuple_impl_ = std::conditional_t<sizeof...(Types) != 1u, tuple_impl_::_indexed_inheritance_node_t<0, Types...>, tuple_impl_::_last_inherited_node_t<0, Types...>>;
		static constexpr bool nothrow_default_constructible = (std::is_nothrow_default_constructible_v<Types> && ...);

		template<typename T>
		static consteval size_t _count_occurrences_of_T() noexcept
		{
			return tuple_impl_::template _count_occurrences_of_T<T>();
		}

		template<size_t IndexToGet>
		constexpr decltype(auto) _get_lvalue_ref() noexcept
		{
			return tuple_impl_::template _get_lvalue_ref<IndexToGet>();
		}

		template<typename Type> requires (::ctl::tuple_impl_::_count_T_in_Types<Type, Types...> == 1)
			constexpr decltype(auto) _get_lvalue_ref() noexcept
		{
			return tuple_impl_::template _get_lvalue_ref<Type>();
		}

		template<size_t IndexToGet>
		constexpr decltype(auto) _get_const_lvalue_ref() const noexcept
		{
			return tuple_impl_::template _get_const_lvalue_ref<IndexToGet>();
		}

		template<typename Type> requires (::ctl::tuple_impl_::_count_T_in_Types<Type, Types...> == 1)
			constexpr decltype(auto) _get_const_lvalue_ref() const noexcept
		{
			return tuple_impl_::template _get_const_lvalue_ref<Type>();
		}

		template<size_t IndexToGet>
		constexpr decltype(auto) _get_rvalue_ref() noexcept
		{
			return tuple_impl_::template _get_rvalue_ref<IndexToGet>();
		}

		template<typename Type> requires (::ctl::tuple_impl_::_count_T_in_Types<Type, Types...> == 1)
			constexpr decltype(auto) _get_rvalue_ref() noexcept
		{
			return tuple_impl_::template _get_rvalue_ref<Type>();
		}

		template<size_t ... I, typename ... Args>
		constexpr void _assign_from(std::index_sequence<I...>, const tuple<Args...>& other)
		{
			((_get_lvalue_ref<I>() = other._get_const_lvalue_ref<I>()), ...);
		}

		template<size_t I, typename ... Args>
		friend constexpr auto& ::ctl::get<I, Args...>(tuple<Args...>& t) noexcept;

		template<size_t I, typename ... Args>
		friend constexpr const auto& ::ctl::get<I, Args...>(const tuple<Args...>& t) noexcept;

		template<size_t I, typename ... Args>
		friend constexpr auto&& ::ctl::get<I, Args...>(tuple<Args...>&& t) noexcept;

		template<typename Type, typename ... Args>
		friend constexpr auto& ::ctl::get<Type, Args...>(tuple<Args...>& t) noexcept;

		template<typename Type, typename ... Args>
		friend constexpr const auto& ::ctl::get<Type, Args...>(const tuple<Args...>& t) noexcept;

		template<typename Type, typename ... Args>
		friend constexpr auto&& ::ctl::get<Type, Args...>(tuple<Args...>&& t) noexcept;

		template<typename Type, typename Tuple> requires _is_tuple_v<Tuple>
		friend struct ::ctl::tuple_impl_::_count_occurrences_of_T_in_Tuple;

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

	template<typename Tuple>
	concept is_tuple = tuple_impl_::_is_tuple_v<Tuple>;

	template<typename ... Types>
	tuple(Types&&...)->tuple<std::decay_t<Types>...>;

	template<typename ... Args>
	constexpr tuple<Args&...> tie(Args& ... args) noexcept
	{
		return tuple<Args&...>(args...);
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

	template<typename Tuple> requires is_tuple<Tuple>
	struct tuple_size : std::integral_constant<size_t, Tuple::size()>
	{

	};

	template<size_t Index, typename First, typename ... Last>
	struct tuple_element<Index, tuple<First, Last...>>
		: tuple_element<Index - 1, tuple<Last...>> {};

	template<typename First, typename ... Last>
	struct tuple_element<0, tuple<First, Last...>>
	{
		using type = First;
	};

	template<size_t Index, typename Tuple> requires (ctl::is_tuple<Tuple> && (ctl::tuple_size_v<Tuple> > Index))
	using tuple_element_t = typename tuple_element<Index, Tuple>::type;

	template<typename Tuple> requires is_tuple<Tuple>
	static constexpr size_t tuple_size_v = tuple_size<Tuple>::value;

	template <typename T, typename Tuple> requires is_tuple<std::decay_t<Tuple>>&& constructible_from_tuple_types<T, Tuple>
	constexpr T make_from_tuple(Tuple&& tuple) noexcept(tuple_impl_::_noexcept_constructible_from_tuple_types<T, Tuple>)
	{
		return[]<size_t ...I>(auto && tuple, std::index_sequence<I...>)
		{
			return T(get<I>(tuple) ...);
		}(std::forward<Tuple>(tuple), std::make_index_sequence<tuple_size_v<std::decay_t<Tuple>>>());
	}

	namespace tuple_static_asserts_
	{
		using trivial_tuple = ctl::tuple<int, double, float, char, void*>;
		using trivial_reference_tuple = ctl::tuple<int&, double&, float&, char&, void*&>;
		using trivial_rvalue_reference_tuple = ctl::tuple<int&&, double&&, float&&, char&&, void*&&>;
		using trivial_const_tuple = ctl::tuple<const int, const double, const float, const char, const void*>;
		using trivial_mixed_tuple = ctl::tuple<int, double&, const float, char&&, const void*&&>;

		static_assert(std::is_trivially_constructible_v<trivial_tuple>);
		static_assert(std::is_trivially_default_constructible_v<trivial_tuple>);
		static_assert(std::is_trivially_copy_constructible_v<trivial_tuple>);
		static_assert(std::is_trivially_move_constructible_v<trivial_tuple>);
		static_assert(std::is_trivially_copyable_v<trivial_tuple>);
		static_assert(std::is_trivially_copy_assignable_v<trivial_tuple>);
		static_assert(std::is_trivially_move_assignable_v<trivial_tuple>);
		static_assert(std::is_trivially_destructible_v<trivial_tuple>);
		static_assert(std::is_trivial_v<trivial_tuple>);

		// get<I> should return the element at index 'I'
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_tuple&>())), float&>);		// get<> from a tuple lvalue reference should return an lvalue reference to the element
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<const trivial_tuple&>())), const float&>); // get<> from a tuple const lvalue reference should return a const lvalue reference to the element
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_tuple&&>())), float&&>);		// get<> from a tuple rvalue reference should return an rvalue reference to the element

		// get<Type> should return the element who's (possibly cv-ref qualified) type matches 'Type'
		// should not compile if the tuple doesn't not contain exactly ONE element of that type 
		static_assert(std::is_same_v<decltype(ctl::get<float>(std::declval<trivial_tuple&>())), float&>);
		static_assert(std::is_same_v<decltype(ctl::get<float>(std::declval<const trivial_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<float>(std::declval<trivial_tuple&&>())), float&&>);

		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_reference_tuple&>())), float&>);
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<const trivial_reference_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_reference_tuple&&>())), float&&>);

		static_assert(std::is_same_v<decltype(ctl::get<float&>(std::declval<trivial_reference_tuple&>())), float&>);
		static_assert(std::is_same_v<decltype(ctl::get<float&>(std::declval<const trivial_reference_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<float&>(std::declval<trivial_reference_tuple&&>())), float&&>);

		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_rvalue_reference_tuple&>())), float&>);
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<const trivial_rvalue_reference_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_rvalue_reference_tuple&&>())), float&&>);

		static_assert(std::is_same_v<decltype(ctl::get<float&&>(std::declval<trivial_rvalue_reference_tuple&>())), float&>);
		static_assert(std::is_same_v<decltype(ctl::get<float&&>(std::declval<const trivial_rvalue_reference_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<float&&>(std::declval<trivial_rvalue_reference_tuple&&>())), float&&>);

		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_const_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<const trivial_const_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<2>(std::declval<trivial_const_tuple&&>())), const float&&>);

		static_assert(std::is_same_v<decltype(ctl::get<const float>(std::declval<trivial_const_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<const float>(std::declval<const trivial_const_tuple&>())), const float&>);
		static_assert(std::is_same_v<decltype(ctl::get<const float>(std::declval<trivial_const_tuple&&>())), const float&&>);

		static_assert(std::is_same_v<decltype(ctl::tie(std::declval<int&>(), std::declval<float&>(), std::declval<std::string&>())), ctl::tuple<int&, float&, std::string&>>);
		static_assert(std::is_same_v<decltype(ctl::tie(std::declval<const int&>(), std::declval<const float&>(), std::declval<const std::string&>())), ctl::tuple<const int&, const float&, const std::string&>>);
		static_assert(std::is_same_v<decltype(ctl::forward_as_tuple(std::declval<int&&>(), std::declval<float&&>(), std::declval<std::string&&>())), ctl::tuple<int&&, float&&, std::string&&>>);
		static_assert(std::is_same_v<decltype(ctl::forward_as_tuple(std::declval<const int&>(), std::declval<const float&>(), std::declval<const std::string&>())), ctl::tuple<const int&, const float&, const std::string&>>);
		static_assert(std::is_same_v<decltype(ctl::make_tuple(std::declval<int&>(), std::declval<float&&>(), std::declval<const std::string&>(), std::declval<std::reference_wrapper<double>>())), ctl::tuple<int, float, std::string, double&>>);

		static_assert(ctl::is_tuple<tuple<int, float, double>>);
		static_assert(ctl::is_tuple<tuple<int, int>>);
		static_assert(ctl::is_tuple<tuple<int>>);
		static_assert(ctl::is_tuple<std::decay_t<volatile tuple<int, float, double>&>>);
		static_assert(ctl::is_tuple<std::decay_t<const tuple<int, float, double>&&>>);

		static_assert(ctl::tuple_size_v<ctl::tuple<int, float, double>> == 3);
		static_assert(ctl::tuple_size_v<ctl::tuple<int, int>> == 2);
		static_assert(ctl::tuple_size_v<ctl::tuple<int>> == 1);

		static_assert(std::is_same_v<ctl::tuple_element_t<0, trivial_mixed_tuple>, int>);
		static_assert(std::is_same_v<ctl::tuple_element_t<1, trivial_mixed_tuple>, double&>);
		static_assert(std::is_same_v<ctl::tuple_element_t<2, trivial_mixed_tuple>, const float>);
		static_assert(std::is_same_v<ctl::tuple_element_t<3, trivial_mixed_tuple>, char&&>);
		static_assert(std::is_same_v<ctl::tuple_element_t<4, trivial_mixed_tuple>, const void*&&>);
	}
}