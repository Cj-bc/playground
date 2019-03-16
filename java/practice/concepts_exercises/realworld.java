interface baseClock {
  void setClock(int h, int m, int s);
  void setAlarm();
  void unsetAlarm();
}

class clock implements baseClock {
  int hour;
  int minute;
  int second;
  boolean is_alarm_set;


  public void setClock(int h, int m, int s) {
    hour = h;
    minute = m;
    second = s;
  }

  public void setAlarm() {
    is_alarm_set = true;
  }

  public void unsetAlarm() {
    is_alarm_set = false;
  }
}


class airConditioner {
  boolean is_on;
  int current_temperature;

  void turnOn() {
   is_on = true;
  }

  void turnOff() {
    is_on = false;
  }

  void setTemperature(int newTmp) {
    current_temperature = newTmp;
  }
}

class realworld {
  public static void main(String[] argv) {
    airConditioner aircon;
    clock cl;
  }
}
