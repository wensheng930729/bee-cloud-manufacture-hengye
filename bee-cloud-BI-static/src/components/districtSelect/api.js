export default {
    findRegionByParentId: {
        api: (pid) =>
          `/supplychainfinance-input/region/findRegionByParentId?pid=${pid}`,
        type: "GET"
    },
}