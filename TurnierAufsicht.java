import java.util.Random;
public class TurnierAufsicht
{
    public int liga(int[] spieletaerkeTeilnehmer)
    {
        int[] siege = new int[spieletaerkeTeilnehmer.length + 1];
        for (int i = 0; i < spieletaerkeTeilnehmer.length; i ++)
        {
            for (int j = i + 1; j < spieletaerkeTeilnehmer.length; j ++)
            {
                double RGN = Math.random();
                int zaehler = spieletaerkeTeilnehmer[i];
                int nenner = spieletaerkeTeilnehmer[i] + spieletaerkeTeilnehmer[j];
                if( RGN < ((double)zaehler / nenner))
                {
                    siege[i + 1] ++;                    
                }
                else
                {
                    siege[j + 1] ++;
                }
            }
        }
        int max = 1;
        for (int i = 1; i <= spieletaerkeTeilnehmer.length; i ++)
        {
            if( siege[max] < siege[i])
            {
                max = i;
            }
        }
        return max;
    } 

    public void ligaAnteil(int[] spieletaerkeTeilnehmer)
    {
        double[] siege = new double[spieletaerkeTeilnehmer.length + 1];
        for(int i = 0; i < 100000; i ++)
        {
            siege[liga(spieletaerkeTeilnehmer)] ++;
        }
        for(int i = 1; i < siege.length; i ++)
        {
            siege[i] /= 1000;
        }
        for(int i = 1; i < siege.length; i ++)
        {
            System.out.print("Spieler " + i + " hat im Durchschnitt " + siege[i] + " % aller Spiele gewonnen.");
            System.out.println(" ");
        }
    }

    public int[][] runde(int[][] spieldaten)
    {
        if( spieldaten.length > 1 )
        {
            int[] siege = new int[spieldaten.length];
            int max = -1;
            for(int i = 1; i < siege.length; i ++)
            {
                siege[i] = 1;
            }
            for(int i = 1; i < spieldaten.length; i += 2)
            {
                double RGN = Math.random();
                int zaehler = spieldaten[i][1];
                int nenner = spieldaten[i][1] + spieldaten[i + 1][1];
                if(RGN < ((double)zaehler / nenner))
                {
                    siege[i + 1] --;
                }
                else
                {
                    siege[i] --; 
                }
            }
            int[][] neueSpieldaten = new int[spieldaten.length / 2 + 1][2];
            int counter = 1;
            for(int i = 0; i < spieldaten.length; i ++)
            {
                if( siege[i] == 1 )
                {
                    neueSpieldaten[counter][0] = spieldaten[i][0];  // Spielernummer
                    neueSpieldaten[counter][1] = spieldaten[i][1];  // Spielstärke
                    counter ++;
                }
            }
            return neueSpieldaten;
        }
        return spieldaten;
    }

    public void ko(int[] spieletaerkeTeilnehmer)

    {        
        int runden = (int)Math.sqrt(spieletaerkeTeilnehmer.length);
        System.out.println(runden);
        int sieger = -1;
        int[][] daten = new int[spieletaerkeTeilnehmer.length + 1][2];
        for( int i = 0; i < spieletaerkeTeilnehmer.length; i ++)
        {
            daten[i + 1][0] = i + 1;
            daten[i + 1][1] = spieletaerkeTeilnehmer[i];
        }
        int[][] spieldaten = daten;
        for(int i = 0; i < runden; i ++)
        {
            spieldaten = runde(spieldaten);
            System.out.println(" ");
            for(int j = 0; j < spieldaten.length; j ++)
            {
                System.out.println(spieldaten[j][0]);
            }
        }
        int[] staerken = new int[2];
        staerken[0] = spieldaten[1][1];
        staerken[1] = spieldaten[2][1];
        sieger = spieldaten[liga(staerken)][0];
        System.out.println("Der Sieger ist der Spieler mit der Nummer: " + sieger + " !" ); 
    }

    public int koXfuenf(int[] spieletaerkeTeilnehmer, int[] turnierplan)
    {
        int[] siege = new int[spieletaerkeTeilnehmer.length + 1];
        int[] siegeGesamt = new int[spieletaerkeTeilnehmer.length + 1];
        int max = -1;
        for(int i = 0; i < spieletaerkeTeilnehmer.length; i += 2)
        {
            for (int runden = 0; runden < 5; runden ++)
            {
                double RGN = Math.random();
                int zaehler = spieletaerkeTeilnehmer[i];
                int nenner = spieletaerkeTeilnehmer[i] + spieletaerkeTeilnehmer[i + 1];
                if(RGN < ((double)zaehler / nenner))
                {
                    siege[i + 1] ++;
                }
                else
                {
                    siege[i + 2] ++; 
                }
            }
            if(siege[i + 1] > siege[i + 2])
            {
                siegeGesamt[i + 1] ++;
            }
            else
            {
                siegeGesamt[i + 2] ++;
            }
        }
        for (int i = 1; i <= spieletaerkeTeilnehmer.length; i ++)
        {
            if( max < siegeGesamt[i]) 
            {
                max = i;
            }
        }
        return max;
    }
}
