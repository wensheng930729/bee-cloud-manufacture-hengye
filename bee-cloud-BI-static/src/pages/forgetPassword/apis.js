export default {
    getReisterCode: {  //获取注册验证码
        api: (phone) => `/supplychainfinance-user/user/getValidateCodeWithHasAccount?phone=${phone}`,
        type: "POST"
    },
    validate: {  //校验验证码
        api: ({ phone, code, type }) => `/supplychainfinance-user/user/validate?account=${phone}&code=${code}&type=${type}`,
        type: "POST"
    },
    resetPassword: {  //重置
        api: () => `/supplychainfinance-user/user/resetPassword`,
        type: "POST"
    },




}