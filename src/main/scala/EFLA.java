public class EFLA implements Drawer<BufferedImage2D> {
    public static final int ROUNDING = 0x8000;
    public static final int FIXEDPOINTNUMBER = 16;

    /**
     * Extremely Fast Line Algorithm Var. E (Addition Fixed Point PreCalc)
     * Copyright 2001-2, By Po-Han Lin
     *
     * Freely usable in non-commercial applications as long as credits
     * to Po-Han Lin and link to http://www.edepot.com is provided in
     * source code and can been seen in compiled executable.
     * Commercial applications please inquire about licensing the algorithms.
     *
     * Latest version at http://www.edepot.com/phl.html
     * This version is for standard displays (up to 65536x65536)
     * @param target
     * @param x0
     * @param y0
     * @param x1
     * @param y1
     * @param color
     */
    @Override
    public void line (BufferedImage2D target, int x0, int y0, int x1, int y1, int color) {
        boolean isDyGreaterThanDx = false;
        int dy = y1 - y0;
        int dx = x1 - x0;
        if (Math.abs (dy) > Math.abs (dx)) {
            int tmp = dy;
            dy = dx;
            dx = tmp;
            isDyGreaterThanDx = true;
        }
        int inc;
        if (dx == 0) {
            inc = 0;
        } else {
            inc = (dy << FIXEDPOINTNUMBER) / dx;
        }

        if (isDyGreaterThanDx) {
            if (dx > 0) {
                dx += y0;
                for (int x = ROUNDING + (x0 << FIXEDPOINTNUMBER); y0 <= dx; ++y0) {
                    target.set (x >> FIXEDPOINTNUMBER, y0, color);
                    x += inc;
                }
                return;
            }
            dx += y0;
            for (int x = ROUNDING + (x0 << FIXEDPOINTNUMBER); y0 >= dx; --y0) {
                target.set (x >> FIXEDPOINTNUMBER, y0, color);
                x -= inc;
            }
            return;
        }

        if (dx > 0) {
            dx += x0;
            for (int y = ROUNDING + (y0 << FIXEDPOINTNUMBER); x0 <= dx; ++x0) {
                target.set (x0, y >> FIXEDPOINTNUMBER, color);
                y += inc;
            }
            return;
        }
        dx += x0;
        for (int y = ROUNDING + (y0 << FIXEDPOINTNUMBER); x0 >= dx; --x0) {
            target.set (x0, y >> FIXEDPOINTNUMBER, color);
            y -= inc;
        }
        return;
    }
}
