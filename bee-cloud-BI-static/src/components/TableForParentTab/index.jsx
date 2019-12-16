import React, { Component } from 'react';
import { Card, Tabs, Form, Button } from 'antd';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';
import { CustomFormHOC } from '@/components/FormWidget';
import StandardTable from '@/components/FormWidget/StandardTable';
import styles from './index.less';

const { TabPane } = Tabs;

const defaultTabs = [
  {
    key: REPORT_TYPE.existingDeatil.key,
    cName: REPORT_TYPE.existingDeatil.cName,
    container: null,
  },
];

const SearchFormHOC = props => {
  const {} = props;
  const SearchFormIndex = SearchFormCom => {
    class NewSearchForm extends Component {
      // 查询按钮 点击事件
      handleSearch = e => {
        const { handleSearch } = this.props;
        e.preventDefault();
        if (handleSearch) {
          handleSearch();
        }
      };

      // 表单查询 改变事件
      handleFormChange = field => {
        const { handleFormChange } = this.props;
        if (handleFormChange) {
          handleFormChange(field);
        }
      };

      render() {
        const { form } = this.props;
        return (
          <Form layout="inline" onSubmit={this.handleSearch}>
            <SearchFormCom {...form} {...this.props} />
            <Form.Item
              wrapperCol={{
                xs: { span: 24, offset: 0 },
                sm: { span: 16, offset: 8 },
              }}
            >
              <Button type="primary" htmlType="submit">
                查询
              </Button>
            </Form.Item>
          </Form>
        );
      }
    }
    return CustomFormHOC(NewSearchForm);
  };

  return SearchFormIndex;
  // <NewSearachForm onChange={this.handleFormChange} />;
};

const TableForParentTab = props => {
  const IndexHOC = SearchFormCom =>
    class Index extends Component {
      static defaultProps = {
        data: { list: [], pagination: {} },
        columns: [],
        currentTab: defaultTabs[0].key,
        parentTabs: defaultTabs,
        childTabs: [],
      };

      constructor(props) {
        super(props);
        this.state = {};
      }

      componentDidMount() {}

      // 父tab改变事件
      handleTabChange = key => {
        const { handleTabChange } = this.props;
        if (handleTabChange) {
          handleTabChange(key);
        }
      };

      // 分页改变事件
      handleTableChange = pagination => {
        const params = {
          currentPage: pagination.current,
          pageSize: pagination.pageSize,
        };
        const { handleTableChange } = this.props;
        if (handleTableChange) {
          handleTableChange(params);
        }
      };

      renderTabs = () => {
        const { parentTabs, childTabs, loading, data, columns, bordered, scroll } = this.props;
        const tableProps = {
          loading,
          columns,
          data,
          onChange: this.handleTableChange,
          bordered,
          scroll,
        };

        if (Array.isArray(parentTabs) && parentTabs.length > 0) {
          return parentTabs.map(item => (
            <TabPane tab={item.cName} key={item.key}>
              <SearchFormCom {...this.props} />
              {this.props.handleBtnAdd && (
                <Button
                  type="primary"
                  onClick={this.props.handleFormAdd}
                  style={{ position: 'absolute', right: 0, top: 0 }}
                >
                  {this.props.handleBtnAddText ? this.props.handleBtnAddText : null}
                </Button>
              )}
              <StandardTable {...tableProps} />
            </TabPane>
          ));
        }
        if (Array.isArray(childTabs) && childTabs.length > 0) {
          const doms = childTabs.map(item => (
            <TabPane tab={item.cName} key={item.key}>
              <StandardTable {...tableProps} />
            </TabPane>
          ));
          return (
            <>
              <SearchFormCom {...this.props} />
              {doms}
            </>
          );
        }
        return (
          <>
            <SearchFormCom {...this.props} />
            <StandardTable {...tableProps} />
          </>
        );
      };

      render() {
        const { currentTab, childTabs, parentTabs } = this.props;
        const bool1 = Array.isArray(childTabs) && childTabs.length > 0;
        const bool2 = Array.isArray(parentTabs) && parentTabs.length > 0;
        return (
          <>
            <Card className={styles.cardContainer}>
              {bool1 || bool2 ? (
                <Tabs activeKey={currentTab} onChange={this.handleTabChange}>
                  {this.renderTabs()}
                </Tabs>
              ) : (
                this.renderTabs()
              )}
            </Card>
          </>
        );
      }
    };

  return IndexHOC;
};

export { TableForParentTab, SearchFormHOC };
