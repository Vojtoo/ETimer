import { Elm } from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('root')
});


app.ports.sendRing.subscribe(()=>{
    const audio = new Audio('bell.mp3');
    audio.play();
});