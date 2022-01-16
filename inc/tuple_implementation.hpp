#pragma once

#include <type_traits>
#include "tuple.hpp"

namespace ctl
{
	template<typename ... Types>
	class tuple;

	namespace tuple_impl_
	{
		template<typename T>
		struct _is_tuple : std::false_type {};

		template<typename... Types>
		struct _is_tuple<ctl::tuple<Types...>> : std::true_type {};

		template<typename T>
		static constexpr bool _is_tuple_v = _is_tuple<T>::value;

		template<typename T, typename ... Types>
		static constexpr size_t _count_T_in_Types = (static_cast<size_t>(std::is_same_v<T, Types>) + ...);

		template<typename Type, typename Tuple> requires _is_tuple_v<Tuple>
		struct _count_occurrences_of_T_in_Tuple : std::integral_constant<size_t, Tuple::template _count_occurrences_of_T<Type>()> {};

		template<typename Type, typename Tuple> requires _is_tuple_v<Tuple>
		static constexpr size_t _count_occurrences_of_T_in_Tuple_v = _count_occurrences_of_T_in_Tuple<Type, Tuple>::value;

		template<typename Type, typename Tuple>
		concept _found_only_once_in_tuple = _is_tuple_v<Tuple> && (_count_occurrences_of_T_in_Tuple_v<Type, Tuple> == 1);

		template<typename T, typename Tuple>
		static constexpr bool _noexcept_constructible_from_tuple_types = []<size_t ... I>(std::index_sequence<I...>) -> bool
		{
			return std::is_nothrow_constructible_v<T, decltype(get<I>(std::declval<Tuple>()))...>;
		}(std::make_index_sequence <tuple_size_v<std::decay_t<Tuple>>>());

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

			template<typename T>
			static consteval size_t _count_occurrences_of_T() noexcept
			{
				if (std::is_same_v<MyType, T>)
					return 1;
				return 0;
			}

			template<typename MyTypeArg> requires std::constructible_from<MyType, MyTypeArg>
			constexpr _last_inherited_node_t(MyTypeArg&& my_value)
				: _value(std::forward<MyTypeArg>(my_value))
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

			template<typename Type> requires std::same_as<MyType, Type>
			constexpr auto& _get_lvalue_ref() noexcept
			{
				return _value;
			}

			template<size_t IndexToGet> requires (IndexToGet == Index)
				constexpr const auto& _get_const_lvalue_ref() const noexcept
			{
				return _value;
			}

			template<typename Type> requires std::same_as<MyType, Type>
			constexpr const auto& _get_const_lvalue_ref() const noexcept
			{
				return _value;
			}

			template<size_t IndexToGet> requires (IndexToGet == Index)
				constexpr auto&& _get_rvalue_ref() noexcept
			{
				return std::move(_value);
			}

			template<typename Type> requires std::same_as<MyType, Type>
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

			template<typename T>
			static consteval size_t _count_occurrences_of_T() noexcept
			{
				if (std::is_same_v<MyType, T>)
					return 1 + _next_inheritence_node::template _count_occurrences_of_T<T>();
				else
					return _next_inheritence_node::template _count_occurrences_of_T<T>();
			}

			template<typename MyTypeArg, typename ... Args>  requires std::constructible_from<MyType, MyTypeArg>
			constexpr _indexed_inheritance_node_t(MyTypeArg&& my_value, Args&& ... others)
				: _next_inheritence_node{ std::forward<Args>(others)... }
				, _value(std::forward<MyTypeArg>(my_value))
			{

			}

			template<size_t Index, typename ... Args>
			constexpr _indexed_inheritance_node_t(std::integral_constant<size_t, Index>, const tuple<Args...>& other)
				: _next_inheritence_node{ std::integral_constant<size_t, Index + 1>{}, other }
				, _value(static_cast<MyType>(other.get<Index>()))
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

			template<typename Type>
			constexpr auto& _get_lvalue_ref() noexcept
			{
				if constexpr (std::is_same_v<Type, MyType>)
				{
					return _value;
				}
				else
				{
					return _next_inheritence_node::template _get_lvalue_ref<Type>();
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

			template<typename Type>
			constexpr const auto& _get_const_lvalue_ref() const noexcept
			{
				if constexpr (std::is_same_v<Type, MyType>)
				{
					return _value;
				}
				else
				{
					return _next_inheritence_node::template _get_const_lvalue_ref<Type>();
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

			template<typename Type>
			constexpr auto&& _get_rvalue_ref() noexcept
			{
				if constexpr (std::is_same_v<Type, MyType>)
				{
					return std::move(_value);
				}
				else
				{
					return _next_inheritence_node::template _get_rvalue_ref<Type>();
				}
			}
		};

		template<typename ... Types>
		using tuple_implementation = std::conditional_t<sizeof...(Types) != 1u, tuple_impl_::_indexed_inheritance_node_t<0, Types...>, tuple_impl_::_last_inherited_node_t<0, Types...>>;
	}
}