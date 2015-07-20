public class DDA implements Drawer<BufferedImage2D> {
    @Override
    public void line (BufferedImage2D target, int x0, int y0, int x1, int y1, int color) {
        int length, i;
        double x, y;
        double xincrement;
        double yincrement;

        length = Math.abs (x1 - x0);
        if (Math.abs (y1 - y0) > length)
            length = Math.abs (y1 - y0);
        xincrement = (double) (x1 - x0) / (double) length;
        yincrement = (double) (y1 - y0) / (double) length;
        x = x1 + 0.5;
        y = y1 + 0.5;
        for (i = 1; i <= length; ++i) {
            try {
                target.set ((int) x, (int) y, color);
            } catch (Exception e) {
                System.out.println (String.format ("%f %f", x, y));
                return;
            }
            x = x + xincrement;
            y = y + yincrement;
        }

        return;
    }
}
