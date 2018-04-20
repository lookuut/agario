<style>
   p {
    text-indent: 20px;
   }
  </style>
# agario

<p>Разработка стратегии в игре Agar IO, в рамках конкурса <a href="https://github.com/MailRuChamps/miniaicups/blob/master/agario/RULES.md">aicups.ru</a>
Суть игры - набрать наибольшее количество очков поедая конкурентов, статичную еду, 
не управляемые выбросы или взрываясь на вирусах, двигаясь в инерционной системе с сопротивлением
изменение скорости задавался формулой:
</p>
speed_x += (nx * max_speed - speed_x) * INERTION_FACTOR / mass;
speed_y += (ny * max_speed - speed_y) * INERTION_FACTOR / mass;


<p>В первые дни быстренько написал стратегию с поиском оптимального пути для поедания еды и конкурентов без учета физики мира, 
стратегия довольно долго держалась в топ 50, дальше начал писать поиск оптимальной траектории с учетом физики и тут меня понесло в дебри.</p>

<p>Вместо того чтобы сесть и вывести формулу направления импульса для достижении нужных координат, 
я написал поиск оптимальной траектории дихотомией угла импульса, метод хорошо работал только при малых кол-вах объектов, 
оно и понятно, кол-во итераций на поиск возрастает экспоненциально, было решено что, 
необходимо предпосчитать углы поворота для дискретных масс - грубо говоря, переходим на полярную систему координат,
делим плоскость на дискретное кол-во углов и дистанций, и относительно стартовой скорости считаем для 
всех направлений позиции нашего объекта в зависимости от времени и в самой стратегии, 
зная относительные координаты объекта мы можем предсказать нужное направление импульса, результат был лучше
и только в конце конкурса, я понял что допустил роковую ошибку не раскрутив до конца формулу скорости.
Из формулы изменения скорости легко вывести формулу скорости для любого на перед заданного времени:</p>

S(t) = S0 * pow(1 - t, t) + P * Vm (1 - I) SUM(i <- 0 to t) pow(1 - I, i)
где S(0) - стартовая скорость
t - время
I - инерция
Vm - максимальная скорость объекта 
P - единичный вектор направление импульса

<p>Допустим мы хотим узнать направления импульса чтобы попасть в точку X, относительно стартовой координаты объекта
=> SUM (i <- to n) S(i) = X 
=>  P = (X - SUM(i <- 0 to t) pow(1 - I, i)) / (Vm * I * SUM(i <- 1 to t) (SUM(j <- 0 to i)pow(1 - I, j))
Конечно, же формула не дает оптимальную траекторию, но все же гораздо лучше чем все время направленный на цель импульс
Но додумался я слишком поздно, только за три часа до закрытия засабмитил стратежку, как итог 122 место, что считаю провалом, но провалом очень поучительным :)
</p>
