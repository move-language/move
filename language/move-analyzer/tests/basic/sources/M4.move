module Basic::M4 {

    fun if_cond(tmp: u64): u64 {

        let tmp = tmp;

        let ret = if (tmp == 7) {
            tmp
        } else {
            let tmp = 42;
            tmp
        };

        ret
    }


}
