for h in $(seq 1 10) $(seq 100 100 1000); do
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory2.prism  --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-2-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory7.prism  --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-7-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory8.prism  --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-8-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory9.prism  --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-9-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory10.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-10-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory11.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-11-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory12.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-12-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory13.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-13-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory15.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-15-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory16.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-16-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory17.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-17-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory18.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-18-${h}.dice
    python /opt/rubicon/rubicon/rubicon.py --prism /opt/rubicon/examples/weatherfactory/weatherfactory19.prism --prop "P=? [F<=${h} \"allStrike\"]" --output ./weather-factory-19-${h}.dice
done