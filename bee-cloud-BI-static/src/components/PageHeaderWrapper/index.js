import React from 'react';
// import { FormattedMessage } from 'umi/locale';
import Link from 'umi/link';
import PageHeader from '@/components/PageHeader';
import { connect } from 'dva';
import GridContent from './GridContent';

const PageHeaderWrapper = ({ children, wrapperClassName, top, ...restProps }) => (
  <div className={wrapperClassName}>
    {top}
    {/* home={<FormattedMessage id="menu.home" defaultMessage="Home" />} */}
    <PageHeader

      key="pageheader"
      {...restProps}
      linkElement={Link}
      itemRender={item => {
        {/* if (item.locale) {
              return <FormattedMessage id={item.locale} defaultMessage={item.title} />;
            } */}
        return item.title;
      }}
    />
    {children ? (
      <div>
        <GridContent>{children}</GridContent>
      </div>
    ) : null}
  </div>
);

export default connect(({ setting }) => ({
}))(PageHeaderWrapper);
