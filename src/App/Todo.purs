module App.Todo where

import Prelude

import Data.Array (concat, snoc, mapWithIndex)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

type Todo =
  { name :: String
  , isDone :: Boolean
  }

type State =
  { count :: Int
  , todos :: Array Todo
  , newTodoText :: String
  }

data Action
  = Increment
  | Decrement
  | AddTodo
  | TextUpdated String
  | ToggleDone Int

todoBaseStyle = "font-size:2rem;margin:1rem 0;padding:1rem;"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0, todos: [], newTodoText: "" }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment, HP.style "color: red;" ]
        [ HH.text "Click me" ]
    , HH.button
        [ HE.onClick \_ -> Decrement ]
        [ HH.text "Decrement" ]
    , HH.button
        [ HE.onClick \_ -> AddTodo ]
        [ HH.text "Add todo" ]
    , HH.input
        [ HE.onValueInput \txt -> TextUpdated txt
        , HP.value state.newTodoText
        ]
    , HH.div_
        ( mapWithIndex
            ( \i todo -> HH.div [ HE.onClick \_ -> ToggleDone i, HPA.role "button", HP.style $ todoBaseStyle <> if todo.isDone then "color:green;" else "color:white;" ]
                [ HH.text $ todo.name ]
            )
            state.todos
        )
    ]

-- todoList state = map (\todo -> HH.div_ [ HH.text $ todo.name ]) state.todos
handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
  Decrement -> H.modify_ \st -> st { count = st.count - 1 }
  AddTodo -> H.modify_ \st -> st { todos = snoc st.todos { name: st.newTodoText, isDone: false }, newTodoText = "" }
  TextUpdated newText -> H.modify_ \st -> st { newTodoText = newText }
  ToggleDone idx ->
    H.modify_ \st -> st
      { todos =
          mapWithIndex
            ( \i todo ->
                if i == idx then
                  todo { isDone = (not todo.isDone) }
                else
                  todo
            )
            st.todos
      }
