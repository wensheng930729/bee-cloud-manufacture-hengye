package com.bee.platform.common.constants.enums;

/**
 * @description: 资源访问拦截等级
 * @author: junyang.li
 * @create: 2018-12-02 16:17
 **/
public enum InterceptorLev {

    /**
     * @notes  资源访问拦截等级
     * @Author junyang.li
     * @Date 15:14 2019/3/19
     **/
    RELEASE(0,"访问放行"),
    INTRANET_RELEASE(1,"登录验证"),
    INTERCEPTOR(2,"权限验证"),
    INTERNAL_ACCESS(3,"仅限内部访问")
    ;

    private Integer key;

    private String val;

    InterceptorLev(){

    }

    InterceptorLev(Integer key, String val){
        this.key=key;
        this.val=val;
    }


    public Integer getKey() {
        return key;
    }

    public void setKey(Integer key) {
        this.key = key;
    }

    public String getVal() {
        return val;
    }

    public void setVal(String val) {
        this.val = val;
    }
}
